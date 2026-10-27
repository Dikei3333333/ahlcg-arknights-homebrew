module Arkham.Homebrew.Hemmer.Investigators.OliviaHemmer (oliviaHemmer) where

import Arkham.Ability
import Arkham.Card
import Arkham.Homebrew.Hemmer.CardDefs.Treacheries qualified as Treacheries
import Arkham.Helpers.Card (cardIsFast, getModifiedCardCost)
import Arkham.Helpers.Cost (getCanAffordCost, getSpendableResources)
import Arkham.Helpers.HealCards (healDamageCardMatcher)
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (cardPlayed, inFastWindow)
import Arkham.I18n
import Arkham.Homebrew.Hemmer.CardDefs.Investigators qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Token (Token (..))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype OliviaHemmer = OliviaHemmer InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

oliviaHemmer :: InvestigatorCard OliviaHemmer
oliviaHemmer =
  investigator OliviaHemmer Cards.oliviaHemmer
    $ Stats {health = 6, sanity = 8, willpower = 3, intellect = 4, combat = 1, agility = 3}

{- | 「无处安放的守护」是否在她的威胁区里。

弱点在场时，她的印刷文本被视作空白（特性除外）——按设计者口径即：
  · 主能力不能给治愈卡 -2 费 / 快速
  · 反应不能结算「队友付 2 资源」
  · 老者签效果不能触发
-}
weaknessInPlay :: InvestigatorAttrs -> Criterion
weaknessInPlay a =
  exists
    $ treacheryIs Treacheries.unsettledGuardian
    <> treacheryInThreatAreaOf a.id

instance HasModifiersFor OliviaHemmer where
  getModifiersFor (OliviaHemmer a) = do
    -- 弱点在场 → 文本空白；本阶段能力已用过 → 不必再提供"潜力"（顺带优化提示）
    blocked <- selectAny $ treacheryIs Treacheries.unsettledGuardian <> treacheryInThreatAreaOf a.id
    available <- selectAny $ AbilityIs (toSource a) 1 <> AbilityWithinLimit (toId a)
    when (not blocked && available) do
      modifySelf a [CanBecomeFast healDamageCardMatcher, CanReduceCostOf healDamageCardMatcher 2]

instance HasAbilities OliviaHemmer where
  getAbilities (OliviaHemmer a) =
    [ -- ① 打出「治愈……伤害」卡时（每阶段一次）：该卡获得快速，或费用 -2
      playerLimit PerPhase
        $ restricted a 1 (Self <> not_ (weaknessInPlay a))
        $ freeReaction
        $ PlayCard #when You (basic healDamageCardMatcher)
    , -- ② 当你治愈**其它**调查员时：其可支付最多 2 资源，换额外治疗
      -- 注意用 NotYou 而不是 Anyone：治疗自己不算（affectsOthers Anyone 会把你也算进去）
      restricted a 2 (Self <> not_ (weaknessInPlay a))
        $ freeReaction
        $ OrWindowMatcher
            [ InvestigatorHealed
                #after
                #damage
                (affectsOthers NotYou)
                (SourceOwnedBy You <> NotSource (SourceIs (toAbilitySource a 2)))
            , InvestigatorHealed
                #after
                #horror
                (affectsOthers NotYou)
                (SourceOwnedBy You <> NotSource (SourceIs (toAbilitySource a 2)))
            ]
    ]

instance HasChaosTokenValue OliviaHemmer where
  getChaosTokenValue iid ElderSign (OliviaHemmer a) | iid == toId a =
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

-- 从窗口栈里找出「被治疗的那个人」；只接受"其它调查员"（治疗自己不算）
getHealedInvestigator :: InvestigatorId -> [Window] -> Maybe InvestigatorId
getHealedInvestigator _ [] = Nothing
getHealedInvestigator self ((windowType -> Window.Healed _ (InvestigatorTarget hid) _ _) : _)
  | hid /= self = Just hid
getHealedInvestigator self (_ : ws) = getHealedInvestigator self ws

instance RunMessage OliviaHemmer where
  runMessage msg i@(OliviaHemmer attrs) = runQueueT $ case msg of
    -- ① 二选一：与查克·费格斯同款写法——先补上"不做就打不出来"的必需项，再给自由选择
    UseCardAbility iid (isSource attrs -> True) 1 ws@(cardPlayed -> card@(PlayerCard pc)) _ -> do
      mCost <- getModifiedCardCost iid card
      for_ mCost \cost -> do
        canAffordCost <- getCanAffordCost iid (CardIdSource pc.id) [#play] ws (ResourceCost cost)
        canAffordActionCost <- getCanAffordCost iid (CardIdSource pc.id) [#play] ws (ActionCost 1)
        requiresFast <- andM [inFastWindow, not <$> cardIsFast card]

        unless (canAffordActionCost && not requiresFast) do
          eventModifier attrs card (BecomesFast FastPlayerWindow)
        unless canAffordCost do
          eventModifier attrs iid (ReduceCostOf (CardWithId card.id) 2)

        when (canAffordCost && canAffordActionCost && not requiresFast) do
          chooseOneM iid do
            (cardI18n $ labeled' "oliviaHemmer.thatEventGainsFast")
              $ eventModifier attrs card (BecomesFast FastPlayerWindow)
            (cardI18n $ labeled' "oliviaHemmer.thatEventCosts2Less")
              $ eventModifier attrs iid (ReduceCostOf (CardWithId card.id) 2)
      pure i
    -- ② 被治疗者决定是否付费；付了才额外治疗
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      for_ (getHealedInvestigator (toId attrs) ws) \hid -> do
        resources <- getSpendableResources hid
        let maxPay = min 2 resources
        when (maxPay > 0) do
          -- 卡面是「其可向你支付」，所以选择权属于被治疗者
          chooseAmount hid "Resources" "Resources" 0 maxPay attrs
      pure i
    ResolveAmounts hid (getChoiceAmount "Resources" -> n) (isTarget attrs -> True) | n > 0 -> do
      -- 资源：被治疗者 → 赫默
      moveTokens (attrs.ability 2) (ResourceSource hid) (toTarget (toId attrs)) #resource n
      -- 每支付 1 资源，额外治疗 1 点伤害/恐惧（自由分配）
      replicateM_ n do
        canDamage <- canHaveDamageHealed (attrs.ability 2) hid
        canHorror <- canHaveHorrorHealed (attrs.ability 2) hid
        when (canDamage || canHorror) do
          chooseOneM hid do
            when canDamage do
              (cardI18n $ labeled' "healDamage") $ healDamage hid (attrs.ability 2) 1
            when canHorror do
              (cardI18n $ labeled' "healHorror") $ healHorror hid (attrs.ability 2) 1
      pure i
    -- ③ 老者签：+2（由 HasChaosTokenValue 给）＋ **可选择**移动到其它调查员所在的已揭示地点
    -- 旧印效果印在调查员身上，不受弱点影响，永远可用；移动是可选项（有"不移动"按钮）。
    ElderSignEffect iid | iid == toId attrs -> do
      locations <- nub . catMaybes <$> for (select NotYou) getLocationOf
      revealed <- filterM (\lid -> lid <=~> RevealedLocation) locations
      when (notNull revealed) do
        chooseOrRunOneM iid do
          labeled' "doNotMove" nothing
          targets revealed \lid -> moveTo (toSource attrs) iid lid
      pure i
    _ -> OliviaHemmer <$> liftRunMessage msg attrs
