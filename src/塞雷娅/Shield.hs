module Arkham.Homebrew.Saria.Assets.Shield (shield) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Helpers.Modifiers hiding (skillTestModifier, skillTestModifiers)
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Homebrew.Saria.CardDefs.Assets qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Shield = Shield AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shield :: AssetCard Shield
shield = asset Shield Cards.shield

-- 永久物、不给自己挂修饰符；但 `IsAsset` 要求必须实现 `HasModifiersFor`
instance HasModifiersFor Shield where
  getModifiersFor (Shield _) = pure ()

instance HasAbilities Shield where
  getAbilities (Shield a) =
    [ -- <强制>（卡面为 ↻）在你的回合结束时，且钙质化不在手牌中：
      --   钙质化在场 → 移除其 1 充能；否则盾牌补 1 充能（技力）
      --
      -- 放在支援卡上而不是调查员卡上：`ControlsThis` 只对 asset/event/skill 源成立
      -- （`Helpers/Criteria.hs` 对 `InvestigatorSource` 恒返回 False），所以这条必须在盾牌自己身上。
      controlled a 1 ControlsThis
        $ forced
        $ TurnEnds #after You
    , -- ⚡ 使用 3 充能（技力）：查找绑定卡牌「钙质化」并加入手牌

      restricted a 2 (ControlsThis <> exists (be a <> AssetWithUseCount Charge (atLeast 3)))
        $ FastAbility (assetUseCost a Charge 3)
    , -- ➡ 攻击：+1 战斗、忽略反击；成功后可选横置盾牌或花 1 充能（技力）使伤害 +1
      controlled a 3 ControlsThis fightAction_
    ]

instance RunMessage Shield where
  runMessage msg a@(Shield attrs) = runQueueT $ case msg of
    -- <强制> 回合结束：钙质化在场则 -1 充能；不在场（且不在手牌）则盾牌 +1 充能
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      inHand <- selectAny $ inHandOf NotForPlay iid <> basic (cardIs Cards.calcification)
      unless inHand do
        calcification <- selectOne $ assetIs Cards.calcification
        case calcification of
          Just cid -> push $ SpendUses (attrs.ability 1) (toTarget cid) Charge 1
          Nothing -> push $ AddUses (toSource (attrs.ability 1)) (toId attrs) Charge 1
      pure a
    -- ⚡ 花 3 充能（技力）把钙质化从绑定牌堆找进手牌
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      bondedCard <- searchBondedJust iid Cards.calcification
      -- 钙质化离场时是走 `PlaceInBonded`（= 放在一边、位于场外、和绑定卡牌一起）回绑定牌堆的，
      -- 引擎会把这张卡同时记进 `gameRemovedFromPlay`（`Game/Runner.hs:2749`）。
      -- 而普通的 `AddToHand` 在 `handleAddToHand`（`Investigator/Runner/Card.hs:1090-1096`）
      -- 里会把 removedFromPlay 的卡**静默过滤掉** —— 这就是「第二次拿不出来」的根因。
      -- `Do (AddToHand …)` 直接走 `handleDoAddToHand`，不做这个过滤（官方内部也用它），
      -- 所以这里用 Do 版本把它拿回手牌。
      push $ Do (AddToHand iid [bondedCard])
      pure a
    -- ➡ 攻击
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 3) iid [SkillModifier #combat 1, IgnoreRetaliate]
      chooseFightEnemy sid iid (attrs.ability 3)
      pure a
    -- 攻击成功：可横置盾牌 / 花 1 充能（技力），使本次攻击伤害 +1
    PassedThisSkillTest iid (isAbilitySource attrs 3 -> True) -> do
      getSkillTestId >>= \case
        Nothing -> pure a
        Just sid -> do
          chooseOneM iid $ cardI18n do
            when (not attrs.exhausted) do
              labeled "shield.exhaustForExtraDamage" do
                exhaustThis attrs
                skillTestModifier sid (attrs.ability 3) iid (DamageDealt 1)
            when (attrs.use Charge > 0) do
              labeled "shield.spendForExtraDamage" do
                push $ SpendUses (attrs.ability 3) (toTarget attrs) Charge 1
                skillTestModifier sid (attrs.ability 3) iid (DamageDealt 1)
            labeled "doNot" nothing
          pure a
    _ -> Shield <$> liftRunMessage msg attrs
