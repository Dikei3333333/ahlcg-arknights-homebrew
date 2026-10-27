module Arkham.Homebrew.Hemmer.Assets.MedicalDrone (medicalDrone) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Homebrew.Hemmer.CardDefs.Assets qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype MedicalDrone = MedicalDrone AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalDrone :: AssetCard MedicalDrone
medicalDrone = asset MedicalDrone Cards.medicalDrone

{- 治疗范围是「探机附属的地点」，不是赫默所在地点；未附属时才跟随控制者。 -}

instance HasAbilities MedicalDrone where
  getAbilities (MedicalDrone a) =
    [ -- ① [fast] 横置 + 2 补给：为探机所在地点所有调查员回复 1 点伤害/恐惧
      restricted a 1 (ControlsThis <> healCriteria a)
        $ FastAbility (exhaust a <> assetUseCost a Supply 2)
    , -- ② 附属到任意已揭示地点；已附属则收回你的控制（这是**行动**，花 1 行动）
      restricted a 2 (ControlsThis <> attachCriteria a)
        $ actionAbility
    ]
   where
    healCriteria x =
      oneOf
        [ exists $ HealableInvestigator (x.ability 1) #damage $ at_ (locationWithAsset x.id)
        , exists $ HealableInvestigator (x.ability 1) #horror $ at_ (locationWithAsset x.id)
        ]
    attachCriteria x = case x.placement of
      AttachedToLocation _ -> NoRestriction
      _ -> exists (RevealedLocation <> LocationCanHaveAttachments)

instance RunMessage MedicalDrone where
  runMessage msg a@(MedicalDrone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mlid <- case attrs.placement of
        AttachedToLocation lid -> pure (Just lid)
        _ -> maybe (pure Nothing) getLocationOf attrs.controller
      for_ mlid \lid -> do
        iids <- select $ investigatorAt lid
        for_ iids \iid' -> do
          canDamage <- canHaveDamageHealed (attrs.ability 1) iid'
          canHorror <- canHaveHorrorHealed (attrs.ability 1) iid'
          when (canDamage || canHorror) do
            chooseOneM iid' do
              when canDamage do
                (cardI18n $ labeled' "healDamage") $ healDamage iid' (attrs.ability 1) 1
              when canHorror do
                (cardI18n $ labeled' "healHorror") $ healHorror iid' (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      case attrs.placement of
        AttachedToLocation _ -> push $ PlaceAsset (toId attrs) (InPlayArea iid)
        _ -> do
          locations <- select $ RevealedLocation <> LocationCanHaveAttachments
          chooseTargetM iid locations \lid ->
            push $ PlaceAsset (toId attrs) (AttachedToLocation lid)
      pure a
    _ -> MedicalDrone <$> liftRunMessage msg attrs
