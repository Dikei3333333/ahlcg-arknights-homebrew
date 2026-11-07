module Arkham.Homebrew.Saria.Assets.Calcification (calcification) where

import {-# SOURCE #-} Arkham.GameEnv (getCurrentBatchId, getPhase)
import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.ForMovement (ForMovement (..))
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.Saria.CardDefs.Assets qualified as Cards
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Phase (Phase (..))
import Arkham.Projection (fieldMap)
import Arkham.Source (Source (..), isAbilitySource)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Calcification = Calcification AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- 卡面（2026-11-07 最终版，用户删掉了「你的回合结束时」那条效果）：

   「绑定（「盾牌」）。使用（3充能）。如果钙质化上没有充能，将其弃掉。
     ↻当前地点或任意连接地点的敌人受到伤害后：使其额外受到1点伤害。
     <强制>敌人阶段中猎手敌人不可移动。
     在钙质化将要进入你的弃牌堆，或被混洗入你的牌库时，不将其放入，
     改为将其放在一边，位于场外（和你的绑定卡牌一起）。」

   - 「没有充能，将其弃掉」用官方 `discardWhenNoUses`（先例：急救 01019）。
   - 「将要进入弃牌堆 → 改为放在一边、位于场外（和绑定卡牌一起）」用 `PlaceInBonded`：
     `Investigator/Runner.hs:2690` 会 `pushAll [When (PlaceInBonded …), Do (PlaceInBonded …)]`，
     `Asset/Runner.hs:709-719` 的 `When` 分支会撤掉排队中的 `Discarded (AssetTarget …)` 并
     `push $ RemoveFromGame`，`Do` 分支把卡塞进 `bondedCards`。
     本卡离场（包括没有充能被弃掉）都经过能力 3 的 `AssetLeavesPlay #when`，在那里先
     `cancelBatch` 再 `PlaceInBonded`，所以它永远不会真的进弃牌堆。
   - 「被混洗入你的牌库」：它离场时已被放在一边，不存在被洗进牌库的时点。
   - 回合结束效果已取消（2026-11-07 晚用户决定；此前试过的两种写法都撤掉，见修复记录 §21）。
-}

calcification :: AssetCard Calcification
calcification = assetWith Calcification Cards.calcification discardWhenNoUses

-- 钙质化所在地点 + 所有连接地点（orConnected_ = 本地点 ∪ 所有连接地点，NotForMovement）
nearbyLocations :: AssetAttrs -> LocationMatcher
nearbyLocations a = orConnected_ (locationWithAsset a.id)

{- 「当前地点或任意连接地点的敌人」，按卡面拆成四类：

   ① 当前地点上的敌人                        → EnemyAt here
   ② 当前地点调查员威胁区里的敌人            → EnemyIsEngagedWith (InvestigatorAt here)
   ③ 所有连接地点上的敌人                    → EnemyAt connected
   ④ 所有连接地点调查员威胁区里的敌人        → EnemyIsEngagedWith (InvestigatorAt connected)

   说明：对「敌人」而言，「在某调查员的威胁区里」=「与该调查员交战」
   （1015 `Matcher.hs:309`：`enemyInThreatAreaOf = enemyEngagedWith`）。

   ⚠️ 为什么 ①②③④ 不能只写 `EnemyAt (here ∪ connected)`：
      1015 的 `EnemyAt` 会主动排除 attached 的敌人
      （`Game.hs:4351-4356`：`if enemy.placement.isAttached then pure False`），
      而敌人一旦与调查员交战，placement 就是 `AttachedTo`（它在威胁区里），
      于是「交战中的敌人」永远不会被 `EnemyAt` 匹配到。
      所以 ②④ 两类必须用「交战」这个 matcher 单独并进来。 -}
enemiesInReach :: AssetAttrs -> EnemyMatcher
enemiesInReach a =
  oneOf
    [ EnemyAt here
    , EnemyIsEngagedWith (InvestigatorAt here)
    , EnemyAt connected
    , EnemyIsEngagedWith (InvestigatorAt connected)
    ]
 where
  here = locationWithAsset a.id
  connected = ConnectedFrom NotForMovement here

{- 从 `DealtDamage` 窗口里扫出「被伤害的敌人」，并**排除钙质化自己打出的那点伤害**：
   能力 1 的补伤害来源是 `AbilitySource (toSource attrs) 1`（`nonAttackEnemyDamage … (attrs.ability 1)`），
   它又会生成 `EnemyDealtDamage` 窗口；如果不排除，补出来的 1 点伤害会再次触发本能力，
   无限套娃（用户 2026-11-07 的第 4 点要求）。
   这里用官方 `isAbilitySource`（`Arkham/Source.hs:328`）判断伤害来源是不是本能力。 -}
dealtDamageEnemyNotFromSelf :: AssetAttrs -> [Window] -> Maybe EnemyId
dealtDamageEnemyNotFromSelf attrs = go . map windowType
 where
  go [] = Nothing
  go (Window.DealtDamage source _ (EnemyTarget eid) _ : rest)
    | not (isAbilitySource attrs 1 source) = Just eid
    | otherwise = go rest
  go (_ : rest) = go rest

instance HasModifiersFor Calcification where
  getModifiersFor (Calcification a) = do
    -- <强制> 敌人阶段中猎手敌人不可移动。
    -- 官方同款「按阶段挂修饰符」写法：02076 VIP区
    --   `phase <- getPhase; modifySelectWhen a (phase == UpkeepPhase) …`
    -- 猎手用 `HunterEnemy`（= `EnemyWithKeyword Keyword.Hunter`，`Matcher/Patterns.hs:212`）。
    -- 注意：卡面没写「当前地点/连接地点」，所以这里是**所有**猎手敌人；
    -- 如果以后想收窄成本地+连接地点，把 `HunterEnemy` 换成 `HunterEnemy <> enemiesInReach a` 即可。
    phase <- getPhase
    modifySelectWhen a (phase == EnemyPhase) HunterEnemy [CannotMove]

instance HasAbilities Calcification where
  getAbilities (Calcification a) =
    [ -- ↻ 当前地点或任意连接地点的敌人受到伤害后：使其额外受到 1 点伤害
      restricted a 1 ControlsThis
        $ freeReaction
        $ EnemyDealtDamage #after AnyDamageEffect (enemiesInReach a) AnySource
    , -- <强制> 当此卡因任意方式离场时，将其放在一边、位于场外（和绑定卡牌一起）
      --   官方同款：ServantOfBrassDaemonaicVassal
      --   `restricted x 2 ControlsThis $ forced $ AssetLeavesPlay #when (be x)`
      --   防递归不在 criteria 里做（`exists`/`cardIs` 在这里会触发 GHC 的类型族报错），
      --   改成在 handler 里检查「卡是否已经在绑定牌堆」：第二次 LeavePlay 窗口时
      --   卡已进 bondedCards，handler 直接什么都不做，递归自然终止
      --   （PlaceInBonded 的 `When` 分支会 RemoveFromGame → RemoveFromPlay → 再开窗口）。
      restricted a 3 ControlsThis
        $ forced
        $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage Calcification where
  runMessage msg a@(Calcification attrs) = runQueueT $ case msg of
    -- ↻ 敌人受伤后补 1 点伤害（不响应钙质化自己补出来的伤害）
    UseCardAbility iid (isSource attrs -> True) 1 ws' _ -> do
      for_ (dealtDamageEnemyNotFromSelf attrs ws') \eid ->
        nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      pure a
    -- <强制> 离场 → 放在一边、位于场外（和绑定卡牌一起）
    --   支援卡的「离场」既可能是被弃（Discard 流程会先排队一条
    --   `Discarded (AssetTarget …)`），也可能是被剧本移除；
    --   两种都经过 `AssetLeavesPlay #when`，在这里统一改成 PlaceInBonded。
    --
    --   2026-11-07 修：这个窗口是在引擎的 `Would` 批次里触发的，批次剩余的消息
    --   （`RemoveFromPlay`、`Discarded …`）还在队列里；不 CancelBatch 的话它们会在
    --   本能力结算完之后继续执行 —— 卡会先进弃牌堆，而我们的 `Do (PlaceInBonded)`
    --   已经跑过（filter 掉了当时的 discardL），于是卡同时出现在
    --   「弃牌堆 / 绑定牌堆 / removedFromPlay」三处（用户 arkham-debug (2) 的现场）。
    --   官方先例：Engineer 51031 `AssetWouldBeDiscarded #when` 的处理里，
    --   第一句就是 `traverse_ cancelBatch =<< getCurrentBatchId`。
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      -- 防递归：卡已经在某人的绑定牌堆里就什么都不做（第二次 LeavePlay 窗口会走到这里）。
      bonded <- maybe (pure []) (\iid -> fieldMap InvestigatorBondedCards id iid) attrs.controller
      unless (any ((== toCardId attrs) . toCardId) bonded) do
        traverse_ cancelBatch =<< getCurrentBatchId
        for_ attrs.controller \iid -> push $ PlaceInBonded iid (toCard attrs)
      pure a
    _ -> Calcification <$> liftRunMessage msg attrs


