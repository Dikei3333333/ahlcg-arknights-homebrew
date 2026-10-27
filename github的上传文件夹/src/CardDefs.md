# 赫默扩展 · 卡面定义（CardDef）

> 引擎仓库里的落点：`Arkham/Homebrew/Hemmer/CardDefs/{Assets,Events,Investigators}.hs`
> （homebrew 目录由 `cards-discover` 预处理自动聚合，不需要改核心文件；见 `../README.md`）

## 1. 调查员：奥利维亚·赫默

放到 `CardDefs/Investigators.hs`：

```haskell
module Arkham.Homebrew.Hemmer.CardDefs.Investigators where

import Arkham.Asset.Cards.Import
import Arkham.Card

oliviaHemmer :: CardDef
oliviaHemmer =
  investigator
    ":hemmer:001"
    ("Olivia Hemmer" <:> "Rhine Life Researcher")
    Seeker
    [Medic, Scholar] -- 医师、学者
```

> 三维/生命/神智不在 CardDef 里，而在实现文件用 `Stats {…}` 给（见 `src/OliviaHemmer.hs`）。
> 参考：`Vincent Lee`（`09004`）的定义就是这个形状。

## 2. 专属支援：医疗探机

放到 `CardDefs/Assets.hs`：

```haskell
module Arkham.Homebrew.Hemmer.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Card

medicalDrone :: CardDef
medicalDrone =
  signature ":hemmer:001" -- 「赫默牌库专用」
    $ (asset ":hemmer:002" ("Medical Drone" <:> "Remote Treatment UAV") 2 Seeker)
      { cdSkills = [#intellect, #intellect, #wild]
      , cdCardTraits = setFromList [Tool, Item, Science]
      , cdUses = uses Supply 8      -- 使用（8 补给）
      , cdUnique = True
      }
      -- 注意：不写 cdSlots —— 无人机不占任何支援槽位
```

> `signature "<调查员卡号>"` 是引擎的「牌库专用」标记（同 `09019 邦妮·沃许` 的写法）。
> `uses Supply 8` 与《外科手术工具》`09056` 的 `cdUses = uses Supply 4` 同款。

## 3. 专属弱点：无处安放的守护（**诡计型弱点**）

放到 `CardDefs/Treacheries.hs`：

```haskell
module Arkham.Homebrew.Hemmer.CardDefs.Treacheries where

import Arkham.Treachery.CardDefs.Import

unsettledGuardian :: CardDef
unsettledGuardian =
  signature ":hemmer:001"
    $ (weakness ":hemmer:003" "Unsettled Guardian")
      { cdCardTraits = setFromList [Flaw]   -- 缺陷（弱点 subtype 由 weakness 构造器负责）
      }
```

> **为什么从「事件」改成「诡计」**：引擎与客户端把"威胁区"当作 **treachery 实体**来渲染
> （`withInvestigatorConnectionData` 里威胁区数据来自 `treacheryInThreatAreaOf`；
> 事件实体则进 `events` 列表，客户端会画在事件/支援区——就是你看到的那个位置）。
> 所以玩家弱点要真的**待在威胁区**，必须用 treachery 的 `weakness` 构造器
> （参考 `08647 附身`、`03019 身份危机`、`01096 失忆`，它们都是 `weakness/basicWeakness` 构造器）。
> 如果坚持印成「事件」，那它只能显示在事件/支援区——引擎没有给事件用的威胁区渲染。
>
> 参考 `03019`：`signature "03002" $ (weakness "03019" "Crisis of Identity") {cdCardTraits = singleton Madness}`。
>
> 实现里用 `placeInThreatArea attrs iid`（treachery 的标准显现写法，同 `08647 附身`），
> 弃掉时走 `toDiscardBy` → 玩家弃牌堆。

## 4. 卡号规划建议

homebrew 卡号用 `":<slug>:<序号>"` 形式（对照 `AgainstTheWendigo` 的 `":against-the-wendigo:030"`）。
建议：

| 卡 | 建议卡号 |
|---|---|
| 奥利维亚·赫默（调查员） | `:hemmer:001` |
| 医疗探机（专属支援） | `:hemmer:002` |
| 无处安放的守护（专属弱点） | `:hemmer:003` |

> 如果模拟器的**卡牌数据库/构筑规则**（`cards_zh-全量.json` 一类）也要登记这些卡，
> 那是另一份数据（你说 arkham.build 那边已经写好了），本仓库不涉及。

## 5. i18n key 清单（新增）

实现代码里用到、需要在 i18n 资源里补的 key：

| key | 用途 |
|---|---|
| `oliviaHemmer.thatEventGainsFast` | 主能力选项：该卡获得快速 |
| `oliviaHemmer.thatEventCosts2Less` | 主能力选项：该卡费用 -2 |
| `label.healDamage` / `label.healHorror` | 标准 key（引擎已有），治疗选择用 |
