module Arkham.Homebrew.Hemmer.Treacheries.UnsettledGuardian (unsettledGuardian) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.HealCards (healDamageCardMatcher)
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.Hemmer.CardDefs.Treacheries qualified as Cards
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype Meta = Meta {healedThisTurn :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UnsettledGuardian = UnsettledGuardian (TreacheryAttrs `With` Meta)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsettledGuardian :: TreacheryCard UnsettledGuardian
unsettledGuardian = treachery (UnsettledGuardian . (`with` Meta False)) Cards.unsettledGuardian

{- 强制①：你打出「治愈……伤害」卡时，额外花 1 行动。

    只在弱点位于威胁区时生效；以修饰符挂在手牌里的目标卡上，
    引擎在可玩性/费用检查时读 AdditionalCost（Helpers/Playable.hs 的 additionalCosts）。

    「把你的调查员视作印刷文本框空白（特性除外）」不在这里实现——
    由赫默自己的能力 criteria 判断弱点是否在场来控制（见 OliviaHemmer.hs）。
-}
instance HasModifiersFor UnsettledGuardian where
  getModifiersFor (UnsettledGuardian (With a _)) = case a.placement of
    InThreatArea iid -> do
      hand <- fieldMap InvestigatorHand id iid
      modifyEach a (filterCards healDamageCardMatcher hand) [AdditionalCost (ActionCost 1)]
    _ -> pure mempty

instance HasAbilities UnsettledGuardian where
  getAbilities (UnsettledGuardian (a `With` _)) =
    [ -- 强制②：你的回合结束时，若本回合没治疗过任何调查员 → 1 点直接恐惧
      restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #at You
    , {- 动作：意志(4) 检定，成功弃掉。

         弱点主人在自己威胁区里当然能做；**其它调查员只要和他同地点也能做**（队友帮过检定）。
      -}
      skillTestAbility $ restricted a 2 discardCriteria actionAbility
    , -- 记账：本回合治疗过任何调查员
      restricted a 3 (InThreatAreaOf You)
        $ forced
        $ OrWindowMatcher
            [ InvestigatorHealed #after #damage Anyone AnySource
            , InvestigatorHealed #after #horror Anyone AnySource
            ]
    ]
   where
    discardCriteria = case a.placement of
      InThreatArea iid ->
        InThreatAreaOf (InvestigatorWithId iid)
          <> youExist (at_ (locationWithInvestigator iid))
      _ -> Never

instance RunMessage UnsettledGuardian where
  runMessage msg t@(UnsettledGuardian (attrs `With` meta)) = runQueueT $ case msg of
    {- 显现：这张弱点直接进入你的威胁区。

       treachery 的「威胁区」是引擎原生支持的（客户端也是按 treachery 实体渲染威胁区），
       `placeInThreatArea` 只改这张卡的 placement；弃掉时经由 Discard 进玩家弃牌堆。
    -}
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    -- 强制②：结算后把"本回合"标记清掉
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      unless (healedThisTurn meta) do
        directHorror iid (attrs.ability 1) 1
      pure $ UnsettledGuardian $ attrs `with` Meta False
    -- 动作：意志 4
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      -- 检定成功才把威胁区里的这张卡弃掉（进玩家弃牌堆）
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    -- 记账
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      pure $ UnsettledGuardian $ attrs `with` Meta True
    -- 兜底：回合开始也清一次标记
    Do BeginRound -> pure $ UnsettledGuardian $ attrs `with` Meta False
    _ -> UnsettledGuardian . (`with` meta) <$> liftRunMessage msg attrs
