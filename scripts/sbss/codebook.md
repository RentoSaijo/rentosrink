# SBSS Codebook

This document describes the shot-by-shot (SBSS) datasets built by:

- [skater.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/sbss/skater.R)
- [goalie.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/sbss/goalie.R)
- [shared.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/sbss/shared.R)

The SBSS pipeline is intentionally tied to the six-partition xG system in:

- [prepare.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/prepare.R)
- [compare.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/compare.R)
- [legacy/clean.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/legacy/clean.R)
- [legacy/train.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/legacy/train.R)

## File Layout

- Skaters aggregate: `data/sbss/skaters_{seasonId}.csv`
- Goalies aggregate: `data/sbss/goalies_{seasonId}.csv`

SBSS is aggregate-only. There are no split per-entity SBSS files.

## General Rules

- Each row is one shot attempt.
- The scripts include both regular season and playoffs because the upstream xG prep filters `gameTypeId %in% 2:3`.
- `gameTypeId` is not written because it is inferable from `gameId`, matching the GBG convention.
- `missed-shot` rows with `reason == "short"` are excluded everywhere. This mirrors the xG training, test, and compare pipelines.
- Regular-season shootout attempts are excluded from SBSS output. Concretely, rows with `gameTypeId == 2` and `periodNumber == 5` are removed before evaluation/output.
- Penalty shots stay in SBSS output. They still score through the `so` xG partition, but their written `strengthState` is forced to `even-strength`.
- Goalie shooters are excluded before output, again matching xG prep.
- SBSS keeps blocked shots, but `xG` is only model-scored for fenwick rows.
- Concretely, blocked-shot rows are retained with `isCorsi = TRUE`, `isFenwick = FALSE`, `isShot = FALSE`, `isGoal = FALSE`, and `xG = 0`.
- Empty-net rows stay in skater SBSS. They only appear in goalie SBSS when `goaliePlayerIdAgainst` is present.
- The output is sorted by entity, then `gameId`, then `eventId`.

## Shot Flags

- `isCorsi`: `eventTypeDescKey` is one of `blocked-shot`, `missed-shot`, `shot-on-goal`, or `goal`.
- `isFenwick`: `eventTypeDescKey` is one of `missed-shot`, `shot-on-goal`, or `goal`.
- `isShot`: `eventTypeDescKey` is `shot-on-goal` or `goal`.
- `isGoal`: `eventTypeDescKey` is `goal`.

The hierarchy is inclusive:

- goals are a subset of shots
- shots are a subset of fenwick
- fenwick is a subset of corsi

## Output Columns

### Skaters

Aggregate file columns:

- `shooterPlayerId`
- `gameId`
- `eventId`
- `strengthState`
- `xCoordNorm`
- `yCoordNorm`
- `isRush`
- `isRebound`
- `isCorsi`
- `isFenwick`
- `isShot`
- `isGoal`
- `xG`
- `goaliePlayerId`
- `goalieTeamId`
- `shooterTeamId`

Split files use the same columns except `shooterPlayerId`.

### Goalies

Aggregate file columns:

- `goaliePlayerId`
- `gameId`
- `eventId`
- `strengthState`
- `xCoordNorm`
- `yCoordNorm`
- `isRush`
- `isRebound`
- `isCorsi`
- `isFenwick`
- `isShot`
- `isGoal`
- `xG`
- `shooterPlayerId`
- `shooterTeamId`
- `goalieTeamId`

Split files use the same columns except `goaliePlayerId`.

## xG Preparation

SBSS uses the same feature-engineering path as xG prep, with one extension: blocked shots are kept in the final attempt table even though the xG models are only scored on fenwick rows.

Shared prep steps:

- Load season play-by-play with `nhlscraper::gc_pbps()`.
- Add shift times via `nhlscraper::add_shift_times()`.
- Add deltas with `nhlscraper::add_deltas()`.
- Add shooter biometrics with `nhlscraper::add_shooter_biometrics()`.
- Add goalie biometrics with `nhlscraper::add_goalie_biometrics()`.
- Normalize `shotType` to `backhand`, `deflected`, `slap`, `snap`, `tip-in`, `wrist`, or `other`.
- Normalize missed-shot reasons into `post`, `high`, `wide`, or `other` for previous-event context.
- Rebuild `typeDescKeyPrev` from the prior event in the same game using the same mapping as xG prep.
- Build skater on-ice lists and shift/rest summaries from the shift-enriched slot columns.
- Coalesce `shootingPlayerId` from `shootingPlayerId` and `scoringPlayerId`.
- Carry the play-by-play `strengthState` forward for output only; if it is missing, output `even-strength`.
- Force penalty-shot output `strengthState` to `even-strength`.

Derived spatial/context features reused from xG:

- `distance`
- `angle`
- all `d*` movement and rate features
- `isBehindNet`
- `crossedRoyalRoad`
- score, shot, fenwick, and corsi state differentials
- shooter biometrics
- goalie biometrics
- shooter shift/rest timing
- on-ice skater shift/rest min/max/avg summaries

## Six-Situation Partitioning

The xG system is six separate models:

- `sd`: standard 5v5, non-empty-net, non-penalty-shot
- `ev`: same skater count, non-standard even strength, non-empty-net, non-penalty-shot
- `pp`: skater advantage, non-empty-net, non-penalty-shot
- `sh`: skater disadvantage, non-empty-net, non-penalty-shot
- `en`: shooter is attacking an empty net
- `so`: penalty shots / shootout-style rows from `situationCode %in% c("1010", "0101")`

Implementation details:

- `is_ps` is checked first from `situationCode`.
- This partitioning is separate from the output `strengthState` column.
- Regular-season shootout rows still join the `so` partition for training, but they are removed before SBSS output and xG evaluation.
- Non-shootout `0101` / `1010` rows are penalty shots; they stay in output and still use the `so` partition.
- `is_en` is checked next from `isEmptyNetAgainst`.
- `sd` is standard 5v5 with both goalies present and both teams at 5 skaters.
- `ev`, `pp`, and `sh` are then assigned from `skaterCountFor` and `skaterCountAgainst`.
- The definitions are intended to be mutually exclusive and collectively exhaustive.

### Missing Situation Inputs

Older seasons can have a small number of rows where `situationCode`, skater counts, or `strengthState` are unavailable.

SBSS follows the legacy xG safeguard:

- after ruling out `so` and `en`, any row missing the strength inputs needed to classify cleanly is forced into `sd`

This matches [legacy/clean.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/legacy/clean.R) and is the main preprocessing difference relative to the current non-legacy prep.

## Predictor Sets By Partition

The canonical column lists live in [prepare.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/prepare.R) inside `get_xg_partition_columns()`.

### Base Predictor Set Used By `sd`

`sd` uses:

- playoff/home/overtime flags
- period and game clock context
- sequence clock context
- `zoneCode`
- `xCoordNorm`, `yCoordNorm`
- movement deltas and per-second movement rates
- `distance`, `angle`
- `isBehindNet`
- `crossedRoyalRoad`
- `typeDescKeyPrev`
- `shotType`
- `isRebound`
- `isRush`
- score state, shot state, fenwick state, and corsi state
- shooter biometrics
- shooter shift/rest timing
- goalie biometrics
- on-ice skater shift/rest min/max/avg summaries for both teams

### Extra Predictors Used By `ev`, `pp`, and `sh`

These three partitions use the full `sd` set plus:

- `isEmptyNetFor`
- `skaterCountFor`
- `skaterCountAgainst`
- `manDifferential`
- `strengthState`

### `en`

`en` starts from the `ev`/`pp`/`sh` predictor set and drops goalie biometrics:

- `goalieHeight`
- `goalieWeight`
- `goalieHandCode`
- `goalieAge`

### `so`

`so` uses a much smaller set:

- playoff/home flags
- `xCoordNorm`
- `yCoordNorm`
- `distance`
- `angle`
- `shotType`
- score, shot, fenwick, and corsi state
- shooter biometrics
- goalie biometrics

## Model Versions And Season Mapping

### Legacy v1 Ridge

Stored model files:

- `models/xG/legacy/{sd,ev,pp,sh,en,so}1.rds`

Stored best-parameter and preprocessing artifacts:

- `models/xG/legacy/results/*1_best_params.csv`
- `models/xG/legacy/results/*1_dummy_levels.csv`
- `models/xG/legacy/results/*1_impute_medians.csv`
- `models/xG/legacy/results/*1_normalize_params.csv`
- `models/xG/legacy/results/*1_unknown_levels.csv`
- `models/xG/legacy/results/*1_novel_levels.csv`
- `models/xG/legacy/results/*1_zero_variance_terms.csv`

Usage in SBSS:

- `2012-13` through `2017-18`: score directly with the stored v1 legacy workflows

### Legacy v2 Ridge

Stored model files:

- `models/xG/legacy/{sd,ev,pp,sh,en,so}2.rds`

Stored best-parameter and preprocessing artifacts:

- `models/xG/legacy/results/*2_best_params.csv`
- matching `dummy_levels`, `impute_medians`, `normalize_params`, `unknown_levels`, `novel_levels`, and `zero_variance_terms` exports

Usage in SBSS:

- `2018-19` through `2022-23`: score directly with the stored v2 legacy workflows

### Current v1 And v2

Current stored model files:

- v1 XGBoost: `models/xG/{sd,ev,pp,sh,en,so}1.rds`
- v2 LightGBM: `models/xG/{sd,ev,pp,sh,en,so}2.rds`

Current best-parameter artifacts:

- `models/xG/results/*1_best_params.csv`
- `models/xG/results/*2_best_params.csv`

### Current v3

The preferred architecture is defined in [compare.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/compare.R):

- `sd`: v1 XGBoost
- `ev`: v2 LightGBM
- `pp`: v1 XGBoost
- `sh`: v2 LightGBM
- `en`: v2 LightGBM
- `so`: v1 XGBoost

Usage in SBSS:

- `2025-26`: score directly with the stored v3 workflows

## Crossfit / Refit Seasons

### `2010-11` And `2011-12`

The stored legacy v1 models are trained on both seasons together, so using them directly on those same seasons would leak target rows into training.

SBSS avoids that by:

- splitting the target season into 5 parts
- checking which xG partitions are actually present in the held-out part
- retraining only the needed legacy ridge partitions on:
  - the other 4 parts of the target season
  - the entirety of the other season in the `2010-11` / `2011-12` pair
- scoring only the held-out part with those temporary models

Implementation note:

- the 5 parts are built at the game level using sorted distinct `gameId` values and `ntile(..., 5)` so entire games stay together

No artifacts are saved for these temporary models.

### `2023-24` And `2024-25`

The same leakage problem exists for the current models, but fully retuning 20 held-out refits across all 6 partitions would be too expensive.

SBSS therefore:

- splits the target season into 5 game-based parts
- checks which xG partitions are present in the held-out part
- uses the stored v3 architecture choice from `compare.R`
- re-fits fresh workflows on:
  - the other 4 parts of the target season
  - the entirety of the paired season
- reuses the already-stored best hyperparameters from `models/xG/results/*_best_params.csv`

Important limitation:

- this still leaks some information because the reused best hyperparameters were originally selected using the full `2023-24` / `2024-25` training pool, including the held-out shots
- the held-out shots are not used in the final model fit for their part, but they did indirectly influence the chosen hyperparameters

## Preprocessing Differences Between Legacy And Current Models

Legacy ridge models:

- use the same partition-specific predictor sets
- convert logical predictors to `yes` / `no` factors
- dummy-code nominal predictors
- median-impute numeric predictors
- remove zero-variance terms
- normalize numeric predictors
- tune ridge penalty

Current XGBoost and LightGBM models:

- use the same partition-specific predictor sets
- convert logical predictors to `yes` / `no` factors
- dummy-code nominal predictors
- median-impute numeric predictors
- remove zero-variance terms
- do not normalize numeric predictors
- tune tree hyperparameters and store the selected best parameters under `models/xG/results`

## Runtime Notes

- Both entry scripts accept `SEASON` from the environment.
- The default `SEASON` is `20252026`.
- Skater and goalie scripts both rebuild the same scored season attempt table and then write different entity views over it.
- No team-level SBSS outputs are written.
