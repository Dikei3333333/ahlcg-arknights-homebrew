module Arkham.Homebrew.Saria.Treacheries.FamilyRelations (familyRelations) where

import Arkham.Ability
import Arkham.Exhaust (mkExhaustion)
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.Saria.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype FamilyRelations = FamilyRelations TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

familyRelations :: TreacheryCard FamilyRelations
familyRelations = treachery FamilyRelations Cards.familyRelations

{- 在威胁区时：
  · 其它调查员的伤害不能再推给塞雷娅 —— 这一半在 Saria.hs 里（弱点在场就不给
    NonDirectDamageMustBeAssignToThisFirst 这个修饰）
  · 塞雷娅不能激活调查员卡上的反应（↻）能力 —— 用 CannotTriggerAbilityMatching 关掉
-}
instance HasModifiersFor FamilyRelations where
  getModifiersFor (FamilyRelations a) = case a.placement of
    InThreatArea iid ->
      modified_ a iid
        $ [ CannotTriggerAbilityMatching
              ( AbilityOnInvestigator (InvestigatorWithId iid)
                  <> AbilityIsReactionAbility
              )
          ]
    _ -> pure mempty

{- 强制能力（2026-11-07 用户改后的卡面）：
   「当你所在地点有调查员受到伤害或恐惧后，横置此卡：你受到 1 点直接伤害和 1 点直接恐惧。」

   1. 触发条件：**你所在地点**的任意调查员（含你自己）受到伤害或恐惧后；
      —— 不再包括「盟友支援」（旧卡面才有），所以只需要一条 `DealtDamageOrHorror`。
   2. 结算效果：**直接**伤害 1 点和**直接**恐惧 1 点（`directDamageAndHorror` =
      `InvestigatorDirectDamage`，不能被盟友/护甲吸收、也不能转嫁）；
   3. 「横置此卡」= 本卡每回合一次：已横置时不再追加（防自触发死循环）。
-}
instance HasAbilities FamilyRelations where
  getAbilities (FamilyRelations a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ DealtDamageOrHorror #after AnySource (InvestigatorAt YourLocation)
    , -- ➡➡：弃掉家庭关系
      restricted a 2 (InThreatAreaOf You)
        $ ActionAbility mempty Nothing (ActionCost 2)
    ]

{- 「横置此卡」= 这张弱点的每回合一次限制：引擎在回合结束会把横置的卡立起来
   （模拟器自己的行为），所以只要卡是立着的，这次伤害/恐惧就把卡横置并追加
   1 伤 + 1 恐；已经横置时不再追加，避免追加的伤害/恐惧又触发自己造成死循环。

   **顺序很关键**：push 进队列是「先 push 先执行」（FIFO，见
   Arkham/Classes/HasQueue.hs 的 runQueueT / pushAll），所以必须**先** push
   横置、**再** push 伤害/恐惧，执行顺序才是「先横置 → 再结算追加的 1 伤 + 1 恐」。
   反过来写（先伤害后横置）的话，追加伤害落地时本卡还没横置，会再次触发这条
   强制能力 → 无限追加伤害、卡永远轮不到横置（m02868 报的就是这个）。 -}
instance RunMessage FamilyRelations where
  runMessage msg t@(FamilyRelations attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    -- 强制（调查员受伤/恐惧）
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      penalty iid
      pure t
    -- ➡➡ 弃掉
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> FamilyRelations <$> liftRunMessage msg attrs
   where
    -- 罚则永远落在「威胁区的主人」头上，而不是触发/被触发的那张卡的主人
    penalty _ =
      case attrs.placement of
        InThreatArea owner | not attrs.exhausted -> do
          -- 先横置，再结算追加的直接伤害/恐惧（push 是 FIFO，先 push 先执行）
          push $ Exhaust (mkExhaustion attrs attrs)
          directDamageAndHorror owner (attrs.ability 1) 1 1
        _ -> pure ()
