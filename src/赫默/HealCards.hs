module Arkham.Helpers.HealCards where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Homebrew.Hemmer.CardDefs.Assets qualified as Hemmer
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Skills

-- | 「治愈……伤害」玩家卡匹配器（72 张）。
--
-- 数据来源：@卡牌实现/_参考导出/治愈伤害卡/卡号-玩家卡.txt@
--   · 83 张命中 → 剔除 6 张调查员 → 合并异画别名 → 再剔除 3 张没有 CardDef 的卡 = 71 张官方卡
--   · 另**手工追加** 1 张自制卡：医疗探机（文本「为当前地点所有调查员回复 1 点伤害和/或恐惧」）
--   · 判定规则见同目录 README：同一句里「治愈」出现在「伤害」之前
--   · 新卡池上线后重跑导出脚本，再重新生成本文件即可（自制卡部分保留在列表末尾）
healDamageCardMatcher :: CardMatcher
healDamageCardMatcher =
  cardsAre
    [ Events.bizarreDiagnosis -- 09046 诡异的诊断
    , Events.bloodWard2 -- 60472 Blood Ward
    , Events.callForBackup2 -- 08129 请求支援
    , Events.cheatDeath5 -- 03310 死里逃生
    , Events.consumeLife -- 60462 Consume Life
    , Events.denyExistence5 -- 05280 否决存在
    , Events.detectivesIntuition -- 12005 Detective's Intuition
    , Events.emergencyAid -- 02105 紧急救援
    , Events.fickleFortune3 -- 09118 命运多舛
    , Events.fieldDressing1 -- 60571 Field Dressing
    , Events.hiddenShelter -- 60565 Hidden Shelter
    , Events.infuseLife -- 60464 Infuse Life
    , Events.leadingLadyFinalGirl -- 90092 Leading Lady (Final Girl)
    , Events.manipulateDestiny2 -- 07162 操纵命运
    , Events.meditativeTrance -- 08061 冥想放空
    , Events.physicalFitness -- 60164 Physical Fitness
    , Events.physicalFitness2 -- 60174 Physical Fitness
    , Events.regurgitation -- 89003 反刍
    , Events.secondWind -- 04149 恢复元气
    , Events.secondWind2 -- 10032 恢复元气
    , Events.shortRest -- 11096 休息片刻
    , Events.soothingMelody -- 05314 舒缓旋律
    , Events.trueAwakening2 -- 11106 真实的觉醒
    , Skills.cosmicGuidance -- 60468 Cosmic Guidance
    , Skills.inspiringPresence -- 03228 先身士卒
    , Skills.inspiringPresence2 -- 11029 身先士卒
    , Assets.alekseySaburovAlwaysOnTheMend -- 12072 Aleksey Saburov
    , Assets.archiveOfConduitsGatewayToParadise4 -- 08044 通路档案
    , Assets.barrierNode -- 11552 Barrier Node
    , Assets.bonesaw -- 09005 骨锯
    , Assets.drMalaSinhaDaringPhysician -- 08717 玛拉·辛哈医生
    , Assets.drMalaSinhaDaringPhysicianResolute -- 08590 玛拉·辛哈医生
    , Assets.earthlySerenity1 -- 08117 尘世宁静
    , Assets.earthlySerenity4 -- 08119 尘世宁静
    , Assets.engineer -- 51031 火车机修工
    , Assets.expeditionGear -- 11761 Expedition Gear
    , Assets.extraRations1 -- 60570 Extra Rations
    , Assets.firstAid -- 01019 急救
    , Assets.firstAid3 -- 03230 急救
    , Assets.girishKadakiaIcpcPunjabDetective4 -- 09038 吉里什·卡达基亚
    , Assets.graysAnatomyTheDoctorsBible5 -- 09058 格雷的医用解剖学
    , Assets.hallowedChalice -- 09084 神圣爵杯
    , Assets.healingWords -- 06163 治愈真言
    , Assets.healingWords3 -- 08068 治愈真言
    , Assets.jessicaHyde1 -- 06118 杰西卡·海德
    , Assets.keyLocusDefensiveBarrier -- 09653 None
    , Assets.ladyEsprit -- 81019 埃斯普利女士
    , Assets.lonnieRitter -- 60309 朗尼·里特纳
    , Assets.ltWilsonStewart -- 85032 威尔逊·斯图尔特中尉
    , Assets.medicalStudent -- 08083 医学生
    , Assets.medicalTexts -- 01035 医学教科书
    , Assets.medicalTexts2 -- 08038 医学教科书
    , Assets.medicoDellaPeste -- 82024 瘟疫医生
    , Assets.painkillers -- 02117 止痛药
    , Assets.petOozeling -- 85030 软泥宠物
    , Assets.preciousMementoFromAFormerLife4 -- 08114 珍贵记忆
    , Assets.preciousMementoFromAFutureLife4 -- 08115 珍贵记忆
    , Assets.purifyingCorruption4 -- 07273 净化腐败
    , Assets.randallCho -- 60102 兰道尔·曹
    , Assets.ravenousMyconidNurturingStrain4 -- 10061 暴食菌株
    , Assets.sinhasMedicalKit -- 08736 辛哈的医药箱
    , Assets.soulSanctification3 -- 09123 至圣灵魂
    , Assets.soundSupport3 -- 11112 坚厚支援
    , Assets.spiritOfHumanity2 -- 07229 人性抉择
    , Assets.spiritualResolve5 -- 06323 摒绝杂念
    , Assets.strangeSolutionRestorativeConcoction4 -- 02262 奇怪的溶液
    , Assets.surgicalKit3 -- 09056 外科手术工具
    , Assets.theBlackCat5 -- 06285 黑猫
    , Assets.theKeyOfSolomonSecretsOfTheUnknown4 -- 10104 所罗门之钥
    , Assets.thermos -- 04274 暖水瓶
    , Assets.wishEater -- 06277 食愿项链
    -- 自制卡（手动追加，重跑导出脚本时不要丢）
    , Hemmer.medicalDrone -- :hemmer:002 医疗探机（文本含「回复 1 点伤害」）
    ]
