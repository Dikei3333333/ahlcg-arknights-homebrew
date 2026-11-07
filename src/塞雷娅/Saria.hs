module Arkham.Homebrew.Saria.Investigators.Saria (saria) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Investigator (canHaveDamageHealed)
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.Saria.CardDefs.Investigators qualified as Cards
import Arkham.Homebrew.Saria.CardDefs.Treacheries qualified as Treacheries
import Arkham.I18n
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Source (Source (..))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Saria = Saria InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

saria :: InvestigatorCard Saria
saria =
  investigator Saria Cards.saria
    $ Stats {health = 9, sanity = 7, willpower = 3, intellect = 4, combat = 5, agility = 3}

{- | 弱点《家庭关系》是否在塞雷娅的威胁区里。

弱点在场时：
  · 她的「非直接伤害先分配给你」失效（其它调查员的伤害不能再推给她）
  · 她不能激活调查员卡上的反应（↻）能力
-}
weaknessInPlay :: InvestigatorAttrs -> Criterion
weaknessInPlay a =
  exists
    $ treacheryIs Treacheries.familyRelations
    <> treacheryInThreatAreaOf a.id

{- 卡面：「任何非直接伤害在分配到你所在地点的其他调查员卡牌之前，必须先分配给你。」

   ⚠ 引擎限制（1015 版本 `Investigator/Runner/Damage.hs` 已核对）：
   `NonDirectDamageMustBeAssignToThisFirst` **只会从「资产」的修饰符里读**
   （`getHealthDamageableAssets` → `getModifiers aid`），调查员卡上挂这个修饰符
   引擎根本不看 —— 所以原来的 `modifySelf a [...]` 是无效的。

   这里改用引擎对调查员唯一支持的「替别人挨伤害」通道（同《汉克·萨姆森》10015）：
   给同地点的其他调查员开 `CanAssignDamageToInvestigator` / `CanAssignHorrorToInvestigator`，
   他们结算伤害时可把伤害改分配给塞雷娅。这是「允许」而不是「强制」，
   若要严格「必须先分配给你」需要打引擎补丁（见 README §6）。
-}
instance HasModifiersFor Saria where
  getModifiersFor (Saria a) = do
    blocked <- selectAny $ treacheryIs Treacheries.familyRelations <> treacheryInThreatAreaOf a.id
    when (not blocked) do
      modifySelect
        a
        (not_ (InvestigatorWithId a.id) <> InvestigatorAt (locationWithInvestigator a.id))
        [CanAssignDamageToInvestigator a.id, CanAssignHorrorToInvestigator a.id]

instance HasAbilities Saria where
  getAbilities (Saria a) =
    [ -- ↻（每回合一次）在伤害分配给你时：取消该伤害中的 1 点；
      -- 若来源是当前地点的敌人，可再对其造成 1 点伤害
      playerLimit PerRound
        $ restricted a 1 (Self <> not_ (weaknessInPlay a))
        $ freeReaction
        $ InvestigatorWouldTakeDamage #when You AnySource IsNonDirectDamage
    ]

instance HasChaosTokenValue Saria where
  getChaosTokenValue iid ElderSign (Saria a) | iid == toId a =
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

-- 从「将要受到伤害」的窗口里取出伤害来源
wouldTakeDamageSource :: [Window] -> Maybe Source
wouldTakeDamageSource [] = Nothing
wouldTakeDamageSource ((windowType -> Window.WouldTakeDamage src _ _ _) : _) = Just src
wouldTakeDamageSource (_ : ws) = wouldTakeDamageSource ws

{- 敌人造成的伤害，来源有两种构造器（1015 `Arkham/Source.hs`）：
     · `EnemyAttackSource eid` —— 敌人攻击造成的伤害（最常见，求生匕首/猎人护甲都按它判断）
     · `EnemySource eid`       —— 其他「由敌人产生」的伤害
   原来只匹配了 `EnemySource`，所以「反击来源敌人」永远不成立（bug #2）。 -}
enemySourceId :: Source -> Maybe EnemyId
enemySourceId = \case
  EnemyAttackSource eid -> Just eid
  EnemySource eid -> Just eid
  _ -> Nothing

instance RunMessage Saria where
  runMessage msg i@(Saria attrs) = runQueueT $ case msg of
    -- ↻ 取消 1 点伤害（并可选反击来源敌人）
    UseCardAbility iid (isSource attrs -> True) 1 ws' _ -> do
      -- Lifted 的 checkAfter 自己就 push 窗口消息（1015 `Message/Lifted/Base.hs:160`），所以先开窗口、再推取消伤害
      checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect (attrs.ability 1) Nothing
      push $ CancelDamage iid 1
      for_ (wouldTakeDamageSource ws' >>= enemySourceId) \eid ->
        whenM (eid <=~> EnemyAt YourLocation) do
          chooseOneM iid $ cardI18n do
            labeled "saria.deal1DamageToSource"
              $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
            labeled "doNot" nothing
      pure i
    -- ★ +2：治愈你所在地点所有调查员和盟友支援 1 点伤害；这些调查员各获得 2 资源、抽 1 张牌
    ElderSignEffect iid | iid == toId attrs -> do
      let src = ElderSignEffectSource iid
          here = locationWithInvestigator iid
      investigators <- select $ InvestigatorAt here
      let ids = investigators :: [InvestigatorId]
      for_ ids \iid' -> do
        canHeal <- canHaveDamageHealed src iid'
        when canHeal $ push $ HealDamage (toTarget iid') src 1
      allies <- select $ HealableAsset src #damage (#ally <> AssetControlledBy (InvestigatorAt here))
      for_ allies \aid -> push $ HealDamage (toTarget aid) src 1
      for_ ids \iid' -> do
        gainResources iid' (attrs.ability 1) 2
        drawCards iid' (attrs.ability 1) 1
      pure i
    _ -> Saria <$> liftRunMessage msg attrs
