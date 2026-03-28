# nhlscraper Ridge xG Model Explanation

This note is the article-oriented companion to [instruction.md](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/instruction.md). The goal here is to explain what the model is, how it was trained, how it is partitioned, what results currently exist, and how an agent on the `nhlscraper` package side can turn this into a pkgdown article.

## Model Intent

The `models/xG/nhlscraper` path rebuilds expected goals for `nhlscraper::calculate_expected_goals()` using ridge logistic regression rather than boosted trees. The motivation is package practicality:

- Ridge logistic regression is much lighter than XGBoost or LightGBM.
- The fitted model can be implemented in the package with base-R math once the coefficients and preprocessing constants are frozen.
- The runtime package code does not need to depend on `tidymodels` or `glmnet`.

The package-facing score is still partition-aware, feature-rich, and grounded in the current public `nhlscraper` play-by-play schema.

## Data Build

Training data is produced from the same shared xG preparation pipeline used by the broader `models/xG` work:

```r
pbp <- nhlscraper::gc_pbps(season) |>
  nhlscraper::add_shift_times(nhlscraper::shift_charts(season)) |>
  nhlscraper::add_deltas() |>
  nhlscraper::add_shooter_biometrics() |>
  nhlscraper::add_goalie_biometrics()
```

The pipeline in [prepare.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/prepare.R) then:

- Keeps the current public-schema names directly, including `eventTypeDescKey`, `periodNumber`, `shotsFor`, `shotDifferential`, `dXCoordNorm`, and `dDistancePerSecond`, while canonicalizing goalie identity from `goalieInNetId` first and `goaliePlayerIdAgainst` second.
- Adds previous-event context (`typeDescKeyPrev`) from the prior event in the same game.
- Builds custom geometry and context features such as `isBehindNet` and `crossedRoyalRoad`.
- Builds shift-based features from on-ice skater slots and shift-time columns.
- Partitions all shots into `sd`, `ev`, `pp`, `sh`, `en`, and `ps`.

## Partition Logic

The partitioning is intentionally explicit and should be mirrored in package code. Any row that still cannot be partitioned after evaluating the six named states should fall back to `sd` rather than being left unscored. In practice this mostly catches legacy rows with missing or malformed skater-count inputs.

```r
is_ps <- situationCode %in% c("1010", "0101")
is_en <- !is_ps & isEmptyNetAgainst
is_sd_standard <- (
  !is_ps & !is_en &
  skaterCountFor == 5 & skaterCountAgainst == 5 &
  !isEmptyNetFor & !isEmptyNetAgainst
)
is_ev <- !is_ps & !is_en & skaterCountFor == skaterCountAgainst & !is_sd_standard
is_pp <- !is_ps & !is_en & skaterCountFor > skaterCountAgainst
is_sh <- !is_ps & !is_en & skaterCountFor < skaterCountAgainst
is_uncategorizable_partition <- !(
  is_ps | is_en | is_sd_standard | is_ev | is_pp | is_sh
)
is_sd <- is_sd_standard | is_uncategorizable_partition
```

Interpretation:

- `sd`: regulation 5v5 without empty nets.
- `ev`: other even-strength states that are not `sd`.
- `pp`: team shooting with a skater advantage.
- `sh`: team shooting short-handed.
- `en`: empty-net-against shots.
- `ps`: penalty-shot and shootout-style events.

`ps` is intentionally simpler than the other partitions. The other five partitions use richer contextual, delta, and shift-timing features.

## Feature Families

The ridge models draw from six broad feature groups:

1. Shot geometry: `xCoordNorm`, `yCoordNorm`, `distance`, `angle`.
2. Event-to-event movement: `dXCoordNorm`, `dYCoordNorm`, `dDistance`, `dAngle`, `dSecondsElapsedInSequence`, plus all four per-second delta columns.
3. Context and state: playoff, home, overtime flags, score state, shots/fenwick/corsi state, skater counts, strength state, zone, and previous-event context.
4. Custom chance descriptors: `isBehindNet`, `crossedRoyalRoad`, `isRebound`, `isRush`.
5. Shooter and goalie biometrics: height, weight, hand, age, and shooter position.
6. Shift timing: shooter shift length, shooter time since last shift, plus team min/max/avg shift burden features.

The exact predictor list, coefficients, medians, normalization constants, and dummy maps for every partition are frozen in [instruction.md](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/instruction.md).

## Training Procedure

Training is implemented in [train.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/train.R) and run from the per-partition entry scripts in the same folder.

Current choices:

- Training seasons: `2023-24` and `2024-25`.
- Response: binary `isGoal`.
- Model: ridge logistic regression via `glmnet` with `mixture = 0`.
- Cross-validation: grouped folds by `gameId` across the full training pool.
- Final fit: refit on all available `2023-24` and `2024-25` rows after selecting the penalty from grouped CV.
- Preprocessing: logical predictors converted to `no` / `yes`, string predictors converted to factors, missing categoricals sent to `unknown`, novel categoricals sent to `new`, one-hot dummy expansion, median imputation for numerics, zero-variance term removal, and z-score normalization for numerics.

One implementation detail matters for article-writing and package implementation: coefficients alone are not enough. The package side also needs the partition-specific dummy maps, medians, means, standard deviations, and zero-variance removals. Those are all exported in [results](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/results).

## Current Cross-Validation Summary

| dataset | partition | seasons | games | rows | goal_rate | best_penalty | cv_log_loss | cv_roc_auc | cv_pr_auc | cv_brier | search_note |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| sd | 5v5 Score/State-Adjusted | 2023,2024 | 2798 | 188930 | 0.0593235589901022 | 0.0000001 | 0.198578681650525 | 0.771783953243345 | 0.980124742617401 | 0.0525321777194838 | accepted_lower_boundary |
| ev | Other Even Strength | 2023,2024 | 1280 | 4907 | 0.111269614835949 | 0.0385662042116347 | 0.331397134078224 | 0.67283516277858 | 0.937637955445998 | 0.0953233729945728 | none |
| pp | Power Play | 2023,2024 | 2793 | 38903 | 0.0972932678713724 | 0.0000001 | 0.303556139340419 | 0.669264538566136 | 0.946342577289955 | 0.0851807617637989 | accepted_lower_boundary |
| sh | Short-Handed | 2023,2024 | 2241 | 5539 | 0.0738400433291208 | 0.0000001 | 0.221051990106459 | 0.796020292767421 | 0.980318920925609 | 0.0627852175246014 | accepted_lower_boundary |
| en | Empty Net Against | 2023,2024 | 1245 | 1828 | 0.573851203501094 | 0.0788046281566992 | 0.619050049619958 | 0.700180975003345 | 0.599215259469518 | 0.216064913970363 | none |
| ps | Penalty Shot | 2023,2024 | 230 | 1188 | 0.315656565656566 | 0.672335753649933 | 0.624135358640592 | 0.526419103398221 | 0.720480968075048 | 0.216268567879362 | none |

The table above is the current training-time summary. These are grouped CV means at the selected penalty, not external test metrics.

## External Test Script

[test.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/test.R) is aligned with [compare.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/compare.R). It is intended to score the saved ridge workflows on:

- `2021-22`
- `2023-24`
- `2025-26`

That makes `2025-26` the actual unseen future season relative to the `2023-24` and `2024-25` training window.

The script mirrors the same metric shape as `compare.R`: rows, goals, total xG, goal rate, xG rate, log loss, Brier score, ROC AUC, PR AUC, calibration ratio, and calibration error.

## External Test Results

The compare-style external run is now complete. [test.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/test.R) wrote:

- [test_by_dataset.csv](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/results/test_by_dataset.csv)
- [test_overall.csv](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/results/test_overall.csv)
- [test_predictions.rds](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/results/test_predictions.rds)

Overall external results by evaluation season:

| season | model | rows | goals | total_xg | goal_rate | xg_rate | log_loss | brier | roc_auc | pr_auc | calibration_ratio | calibration_error |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2021-22 | ridge_glmnet | 122341 | 8936 | 9258.57878097065 | 0.0730438688541061 | 0.0756774251285842 | 0.231567980277077 | 0.0624904433611446 | 0.746283062034699 | 0.223554913221832 | 1.03632371877917 | 0.0363237187791708 |
| 2023-24 | ridge_glmnet | 122180 | 8771 | 8734.33625114591 | 0.0717881813717466 | 0.0714887566757717 | 0.222168683942953 | 0.0605159579505294 | 0.777506473295121 | 0.244399196362704 | 0.995822626171237 | 0.00417737382876272 |
| 2025-26 | ridge_glmnet | 74169 | 5521 | 5777.87174142816 | 0.0744354162822734 | 0.0778983594911791 | 0.231862123052462 | 0.0632027731918452 | 0.761722443358218 | 0.234128659171533 | 1.04652576624582 | 0.0465257662458153 |

The `2025-26` row above is the actual unseen-future result relative to the `2023-24` and `2024-25` training window.

`2025-26` by partition:

| dataset | rows | goals | total_xg | goal_rate | xg_rate | log_loss | brier | roc_auc | pr_auc | calibration_ratio | calibration_error |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| en | 604 | 328 | 331.786717236475 | 0.543046357615894 | 0.549316750391515 | 0.595858199851634 | 0.206345636609383 | 0.740004591368228 | 0.77426727986404 | 1.01154486901364 | 0.0115448690136433 |
| ev | 1750 | 182 | 208.980988174324 | 0.104 | 0.119417707528185 | 0.310907739234827 | 0.0886110295363554 | 0.702086855080325 | 0.218766741138551 | 1.14824718777099 | 0.14824718777099 |
| pp | 12489 | 1192 | 1289.45469974658 | 0.0954439915125318 | 0.103246913987723 | 0.304484173590019 | 0.0844276919723858 | 0.651700396355764 | 0.155445907467866 | 1.08175729844512 | 0.0817572984451175 |
| sd | 57157 | 3523 | 3637.01678859239 | 0.0616379456514509 | 0.0636323538943026 | 0.205612745001144 | 0.0548317668556206 | 0.761534217696758 | 0.159162683238108 | 1.03236468057092 | 0.0323646805709239 |
| sh | 1610 | 112 | 132.577689963188 | 0.0695652173913043 | 0.0823463912814831 | 0.219823116893889 | 0.0624490083401214 | 0.784393238434164 | 0.164424527473264 | 1.18372937467132 | 0.183729374671323 |
| ps | 559 | 184 | 177.055350493301 | 0.329159212880143 | 0.316736762957604 | 0.633589954411776 | 0.220856461776991 | 0.513107629992654 | 0.330673865549001 | 0.962257339637503 | 0.0377426603624969 |

## Practical Interpretation

- The ridge rebuild is partition-specific rather than one-size-fits-all.
- The package model still uses rich spatial, temporal, contextual, and player-biometrics features.
- `sd`, `pp`, and `sh` still prefer almost no shrinkage under grouped CV, while `ev`, `en`, and especially `ps` prefer more noticeable regularization.
- `sd` remains the dominant high-volume partition, and the unseen-future `2025-26` test kept it reasonably calibrated with `xG / goals = 1.0324` and `ROC AUC = 0.7615`.
- Overall `2025-26` calibration is slightly high at `1.0465`, driven mostly by `ev`, `pp`, and `sh` overprediction.
- The strongest `2025-26` discrimination among non-empty-net hockey states is still `sh` by ROC AUC, but its sample is much smaller than `sd`.
- `ps` remains structurally different and less stable; its future-season ROC AUC was `0.5131`.

## Suggested Article Framing

If another agent is writing the pkgdown article, the most defensible structure is:

1. Explain why `nhlscraper` uses ridge logistic regression instead of heavier model classes at package runtime.
2. Show the partition logic first, because the model is really six models rather than one.
3. Explain the feature families in human terms: geometry, movement, context, biometrics, shift load.
4. Describe the training regime honestly: grouped CV on all `2023-24` and `2024-25` rows, followed by a full-data final fit.
5. Report the grouped-CV summary only as a training diagnostic, not as an unseen-future claim.
6. Use the `2025-26` rows from [test_overall.csv](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/results/test_overall.csv) and [test_by_dataset.csv](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/results/test_by_dataset.csv) for all unseen-future statements.
7. Link readers to [instruction.md](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/instruction.md) for exact coefficients and preprocessing constants.

## Files To Hand Off

- [instruction.md](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/instruction.md): exhaustive implementation spec with coefficients and preprocessing constants.
- [explanation.md](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/explanation.md): narrative explanation and current grouped-CV summary for the article side.
- [test.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/test.R): compare-style external evaluation script for `2021-22`, `2023-24`, and `2025-26`.
- [results](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/results): all CSV and RDS artifacts used by both markdown files.
