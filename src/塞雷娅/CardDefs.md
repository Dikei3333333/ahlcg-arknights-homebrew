# 塞雷娅扩展 · 卡面定义（CardDef）

> 引擎落点：`Arkham/Homebrew/Saria/CardDefs/{Investigators,Assets,Treacheries}.hs`
> **卡号以 ContentPackage 里的 arkhambuild JSON 为准**（`塞雷娅_arkhambuild.json`，
> 客户端已按这些卡号生成卡图/组卡器记录；引擎 CardDef 必须用同一串，否则卡面对不上）：

| 卡 | 卡号（JSON `code`） | 类型 / 阵营 | 其他 |
|---|---|---|---|
| 塞雷娅（调查员） | `63F18C15` | investigator / guardian | unique，3/4/5/3，9 血 7 神 |
| 「盾牌」（专属支援，永久） | `43194663` | asset / neutral | permanent、unique、slot Hand、使用 0 充能 |
| 钙质化（绑定支援） | `3EC5E304` | asset / neutral | cost 0、使用 3 充能、无槽位 |
| 家庭关系（专属弱点） | `5AA84167` | treachery / neutral | subtype weakness、trait 局势 |

> ⚠️ 盾牌/钙质化/家庭关系在 JSON 里都是 **neutral**（不是 Guardian），CardDef 的阵营参数要写 `Neutral`。

---

## 1. 调查员：塞雷娅

`CardDefs/Investigators.hs`：

```haskell
module Arkham.Homebrew.Saria.CardDefs.Investigators where

import Arkham.Card

saria :: CardDef
saria =
  investigator
    "63F18C15"
    ("Saria" <:> "Former Rhine Lab Defense Director")
    Guardian
    [Warden, Scholar, Medic]   -- 监守 / 学者 / 医师
```

- 四维 3/4/5/3、生命 9 / 神智 7 写在实现的 `Stats {…}` 里（不在 CardDef）。
- `Warden`(监守)、`Scholar`(学者)、`Medic`(医师) 都是引擎已有的 Trait（`Arkham/Trait.hs`）。
- 反应（↻）卡面上有「（每回合一次）」：`playerLimit PerRound` 写在**实现**的 `HasAbilities` 里，CardDef 不用加字段。
- 背面组卡规则（牌库 35 张、构筑选项、需求「盾牌/家庭关系/1 张随机基础弱点」、无违法卡牌）属于**客户端组卡器**规则，
  引擎侧保持 `cdDeckRestrictions = []`；签名卡与弱点的绑定关系由 `signature`/`weakness` + `cdBondedWith` 表达。

## 2. 「盾牌」（专属支援，永久 + 绑定钙质化）

`CardDefs/Assets.hs`：

```haskell
module Arkham.Homebrew.Saria.CardDefs.Assets where

import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Slot (Slot (..))
import Arkham.Token (Token (..))

shield :: CardDef
shield =
  signature "63F18C15"        -- 塞雷娅牌库专用（调查员卡号）
    $ permanent               -- 永久：开局自动入场、不进牌库
      $ (asset "43194663" ("Shield" <:> "Originium Arts Staff") 0 Neutral)
        { cdCardTraits = setFromList [Item, Weapon, Melee, Warden]
        , cdUnique = True
        , cdSlots = [#hand]                     -- 占 1 个手部槽位
        , cdBondedWith = [(1, "3EC5E304")]      -- 绑定 1 张《钙质化》
        , cdUses = uses Charge 0               -- 技力 = 引擎自带「充能」token（初始 0，靠回合末反应累加）
        }
```

- 卡面没有技能图标 ⇒ 不写 `cdSkills`；**占 1 个手部槽位**（`cdSlots = [#hand]`，`#hand` 来自 `Arkham.Slot`）。
- 「永久」用 `permanent` 包装（引擎在组牌阶段把 `cdPermanent` 的卡从牌库拿出、开局直接入场，
  见 `Investigator/Runner.hs` 的 `permanentCards` 逻辑）——这样"开始游戏时把盾牌放置入场"自动满足。
- 绑定关系写在**来源卡**上：`cdBondedWith = [(1, "3EC5E304")]`（同《梦境日记》`cdBondedWith = [(1, "06113")]`）。
  《钙质化》因此开局在绑定牌堆，可用盾牌的 ⚡ 能力找回来。
- 阵营按 JSON 写 `Neutral`（客户端 DB 里盾牌是 neutral）。
- 「技力」直接用引擎自带的**充能 token**（`Token.Charge`，`cdUses = uses Charge 0`），不再用 `Resource`：
  卡上初始 0 个充能，回合末反应 `AddUses … Charge 1` 累加；客户端显示为标准的「充能」图标，无需任何自定义 token 声明。

## 3. 钙质化（绑定支援，使用 3「充能（阻回）」）

```haskell
calcification :: CardDef
calcification =
  (asset "3EC5E304" "Calcification" 0 Neutral)
    { cdSkills = [#wild, #wild, #wild, #wild]   -- 卡面 4 个"?"
    , cdCardTraits = setFromList [Spell, Warden] -- 法术 / 守护
    , cdUses = uses Charge 3                   -- 使用（3 充能）
    }
```

> **2026-11-07 新版卡面改法**：「没有充能 → 将其弃掉」用官方的
> `assetWith Calcification Cards.calcification discardWhenNoUses`
> （`Arkham/Asset/Types.hs:588-589`；先例：急救 01019）。离场后**不再用
> `cdWhenDiscarded = ToBonded`**（这条只在 `AddToDiscard` 里生效，支援卡的普通弃牌走的是
> `Discarded` → `handleDiscarded`，根本不看 `cdWhenDiscarded`，所以之前那版在实机上不生效）。
>
> 「将要进入弃牌堆 → 改为放在一边、位于场外（和你的绑定卡牌一起）」用引擎的
> `PlaceInBonded`：`Investigator/Runner.hs:2690` 会 `pushAll [When (PlaceInBonded …), Do (PlaceInBonded …)]`；
> `Asset/Runner.hs:709-719` 的 `When` 分支会撤掉排队中的 `Discarded (AssetTarget …)` 并
> `push $ RemoveFromGame`，`Do` 分支把卡放进 `bondedCards` —— 合起来正好是「放在一边、位于场外、
> 和绑定卡牌一起」。官方先例：皇后的挂坠 06022（`SpentAllUses … -> placeInBonded controller attrs`）。
> 钙质化离场时会经过能力 3 的 `AssetLeavesPlay #when`，在那里 **先 `cancelBatch` 掉当前
> `Would` 批次、再 `PlaceInBonded`**。只 `PlaceInBonded` 是不够的：`Discard` 流程把
> `[RemoveFromPlay, Discarded …]` 排在同一个 `Would` 批次里（`Asset/Runner.hs:666-687`），
> 我们的 `Do (PlaceInBonded)` 先跑完、批次里剩下的 `Discarded` 再执行 → 卡会同时出现在
> 「弃牌堆 / 绑定牌堆 / removedFromPlay」三处（用户 2026-11-07 的 `arkham-debug (2)` 现场）。
> 官方同款写法：Engineer 51031 的 `AssetWouldBeDiscarded #when` 处理里第一句就是
> `traverse_ cancelBatch =<< getCurrentBatchId`（`Arkham/Message/Lifted.hs:3476-3477`）。
> 「被混洗入牌库」同理（离场时已被放在一边，不存在被洗进牌库的时点）。
>
> ⚠️ **副作用（已知）**：卡被放进 `bondedCards` 的同时，引擎会把它记进 `gameRemovedFromPlay`
> （`Game/Runner.hs:2749` `RemoveFromGame (AssetTarget …)`）。所以盾牌找它回手时不能直接用
> `AddToHand`（`handleAddToHand` 会静默过滤 removed 的卡，`Investigator/Runner/Card.hs:1090-1096`），
> 要用 `Do (AddToHand …)` 走 `handleDoAddToHand`（官方内部同样用它）。
> 若想让引擎更干净，可以让引擎侧给一个通用小补丁：`handleAddToHand` 的 removed 过滤里放行
> 「仍在某调查员 `bondedCards` 里的卡」。不补丁也能玩。
>
> **2026-11-07（第五版）**：用户决定**取消**钙质化「你的回合结束时」那条效果（卡面已删），
> 所以钙质化现在只有：能力 1（敌人受伤追加 1 伤）、能力 3（任意离场 → 放在一边、位于场外、和绑定牌一起）。
> 之前试过的两种写法（支援卡上的可点击反应；挪到调查员卡上的 `forced`）都已撤掉。
> 盾牌自己的回合结束充能改为**强制**并留在盾牌卡上：
> `controlled a 1 ControlsThis $ forced $ TurnEnds #after You`
> （`ControlsThis` 对 `InvestigatorSource` 恒为 false，见 `Helpers/Criteria.hs`，所以调查员卡不能用它）。

- 「阻回」**直接复用引擎自带的充能 token**（`Token.Charge`，`Arkham/Token.hs` 的 `| Charge`），
  即卡面「阻回」在引擎里就是「充能」：`cdUses = uses Charge 3`，实现里用 `SpendUses … Charge 1` 扣、
  `AssetWithUseCount Charge` 数。不需要 `Token.Custom`，也不需要 `SetDefs` 声明或任何 i18n，
  客户端会按标准充能图标显示数量。
- 卡面无槽位 ⇒ 不写 `cdSlots`。

## 4. 家庭关系（专属弱点，诡计）

`CardDefs/Treacheries.hs`：

```haskell
module Arkham.Homebrew.Saria.CardDefs.Treacheries where

import Arkham.Treachery.CardDefs.Import

familyRelations :: CardDef
familyRelations =
  signature "63F18C15"
    $ (weakness "5AA84167" "Family Relations")
```

- 玩家弱点必须是**诡计（treachery）**才能进威胁区（客户端按 treachery 实体渲染威胁区；
  事件实体会被画到事件/支援区）。参考 `03019 身份危机`、`08647 附身`。
- 卡号 `5AA84167`、阵营 `neutral`、`cdCardSubType = Just Weakness`（弱点）。
- 卡面特性是「**局势**」（Situation）：上游 `Arkham/Trait.hs`（main）**没有 `Situation`** 构造器，两种处理：
  1. 先不写特性（不影响玩法，只影响卡面/筛选显示）；
  2. 在引擎 `Arkham/Trait.hs` 里加一行 `| Situation`，再用
     ```haskell
     { cdCardTraits = setFromList [Situation] }
     ```
- 最新卡面的强制改为「**你所在地点的任意调查员或盟友支援**受到伤害/恐惧后，横置此卡：你受到 1 点伤害**和** 1 点恐惧」，
  已在实现里用两条 `forced`（`DealtDamageOrHorror` 盯调查员、`AssetDealtDamageOrHorror` 盯盟友）实现；
  「横置此卡」的每回合一次限制直接用引擎横置状态（引擎在回合结束重置横置卡），CardDef 不用加字段。
- 「➡➡ 弃掉」是 2 行动的启动能力（实现里的 `ActionAbility mempty Nothing (ActionCost 2)`），CardDef 不用加字段。

---

## 5. i18n key 清单（新增）

| key | 用途 |
|---|---|
| `saria.deal1DamageToSource` | 塞雷娅反应：对伤害来源敌人造成 1 点伤害 |
| `doNot` | 通用「不」选项（引擎已有标准 key） |
| `shield.exhaustForExtraDamage` | 盾牌：横置盾牌使本次攻击伤害 +1 |
| `shield.spendForExtraDamage` | 盾牌：花 1 充能（技力）使本次攻击伤害 +1 |
