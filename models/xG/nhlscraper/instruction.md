# nhlscraper Ridge xG Implementation Instruction

This document is the package-handoff spec for implementing the ridge-based xG models trained from `models/xG/data/*_train.csv` into the `nhlscraper` R package without requiring `tidymodels` or `glmnet` at runtime.

## Training Scope

- Training seasons: `2023-24` and `2024-25`.
- Internal training regime: grouped cross-validation by `gameId` across the full training pool. There is no longer any internal `2024-25` tail holdout in this repo.
- External holdout: `2025-26` via [test.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/test.R), aligned with [compare.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/compare.R).
- Model family: ridge logistic regression (`glmnet`, `mixture = 0`).
- Preprocessing during training: string-to-factor, `unknown` fill for missing categoricals, `new` bucket for novel categoricals, one-hot dummying, median imputation for numerics, zero-variance removal, z-score normalization for numerics.
- Runtime target: reproduce scoring in package code using current `nhlscraper` public play-by-play columns and base-R math only.

## Required Data Pipeline

The package implementation should start from the current public schema and then add the same helper columns used in training:

```r
pbp <- nhlscraper::gc_pbps(season) |>
  nhlscraper::add_shift_times(nhlscraper::shift_charts(season)) |>
  nhlscraper::add_deltas() |>
  nhlscraper::add_shooter_biometrics() |>
  nhlscraper::add_goalie_biometrics()
```

Required public-schema columns expected by the model builder:

- `eventTypeDescKey`, `goaliePlayerIdAgainst`, `periodNumber`, `shotsFor`, `shotsAgainst`, `shotDifferential`
- `dXCoordNorm`, `dYCoordNorm`, `dDistance`, `dAngle`, `dSecondsElapsedInSequence`
- `dXCoordNormPerSecond`, `dYCoordNormPerSecond`, `dDistancePerSecond`, `dAnglePerSecond`

## Partition Logic

Use the exact partition logic below before scoring. For legacy rows that still do not have enough strength-state information to map cleanly, force the row into `sd` after ruling out `so` and `en`.

```r
is_ps <- situationCode %in% c("1010", "0101")
is_en <- !is_ps & isEmptyNetAgainst
is_unclassifiable_strength <- !is_ps & !is_en & (
  is.na(situationCode) |
  is.na(skaterCountFor) |
  is.na(skaterCountAgainst)
)
is_sd <- (
  !is_ps & !is_en &
  skaterCountFor == 5 & skaterCountAgainst == 5 &
  !isEmptyNetFor & !isEmptyNetAgainst
) | is_unclassifiable_strength
is_ev <- !is_ps & !is_en & skaterCountFor == skaterCountAgainst & !is_sd
is_pp <- !is_ps & !is_en & skaterCountFor > skaterCountAgainst
is_sh <- !is_ps & !is_en & skaterCountFor < skaterCountAgainst

partition <- dplyr::case_when(
  is_ps ~ "so",
  is_en ~ "en",
  is_sd ~ "sd",
  is_ev ~ "ev",
  is_pp ~ "pp",
  is_sh ~ "sh",
  TRUE ~ NA_character_
)
```

This fallback exists because older source seasons can contain a very small number of shot rows with missing `situationCode` and skater-count fields. The package implementation should still emit an xG for those rows instead of dropping them.

## Engineered Features Required Before Scoring

- `typeDescKeyPrev`: previous-event descriptor built from the prior event in the same game, with the same mapping logic used in [prepare.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/prepare.R).
- `isBehindNet`: `!is.na(xCoordNorm) & xCoordNorm >= 89`.
- `crossedRoyalRoad`: previous normalized y-coordinate and current normalized y-coordinate are on opposite sides of 0 using `yCoordNorm - dYCoordNorm`.
- Shift summary features: min/max/avg shift time and since-last-shift time for both teams, excluding goalies.
- Shooter-specific shift features: `shooterSecondsElapsedInShift`, `shooterSecondsElapsedSinceLastShift`.
- `shootingPlayerId`: `coalesce(shootingPlayerId, scoringPlayerId)`.
- `shotType`: normalize to `backhand`, `deflected`, `slap`, `snap`, `tip-in`, `wrist`, else `other`.

## Runtime Preprocessing Contract

- Convert logical predictors to character/factor values `no` / `yes` before dummying.
- For nominal predictors, replace missing values with `unknown`.
- For values not seen during training, route to the `new` bucket if that bucket exists for the variable; otherwise leave all dummies for that variable at `0`.
- Create dummy columns using the exact `output_column` names listed in each dummy map table below.
- Median-impute numeric predictors using the partition-specific imputation table.
- Remove any dummy or numeric columns listed in the zero-variance table before scoring.
- Normalize numeric predictors with `(x - mean) / sd` using the partition-specific normalization table.
- Score with `eta = intercept + sum(beta_i * x_i)` and `xG = plogis(eta)`.

## Package-Side Skeleton

The runtime package implementation can stay light. The scoring path only needs the engineered features plus the frozen per-partition specs exported in [results](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/results).

```r
score_ridge_partition <- function(df, spec) {
  if (nrow(df) == 0L) return(numeric(0))

  for (nm in names(spec$impute_medians)) {
    if (!nm %in% names(df)) df[[nm]] <- NA_real_
    miss <- is.na(df[[nm]])
    df[[nm]][miss] <- spec$impute_medians[[nm]]
  }

  design <- list()

  for (var in names(spec$dummy_map)) {
    values <- as.character(df[[var]])
    values[is.na(values)] <- "unknown"
    known_levels <- names(spec$dummy_map[[var]])
    novel_idx <- !(values %in% c(known_levels, "unknown"))
    if ("new" %in% known_levels) values[novel_idx] <- "new"

    for (lvl in names(spec$dummy_map[[var]])) {
      col_name <- spec$dummy_map[[var]][[lvl]]
      design[[col_name]] <- as.numeric(values == lvl)
    }
  }

  numeric_terms <- intersect(names(spec$normalize_means), names(df))
  for (nm in numeric_terms) {
    design[[nm]] <- (df[[nm]] - spec$normalize_means[[nm]]) / spec$normalize_sds[[nm]]
  }

  for (nm in spec$zero_variance_terms) design[[nm]] <- NULL

  eta <- rep(spec$coefficients[["(Intercept)"]], nrow(df))
  coef_terms <- setdiff(names(spec$coefficients), "(Intercept)")
  for (nm in coef_terms) {
    value <- design[[nm]]
    if (is.null(value)) value <- rep(0, nrow(df))
    eta <- eta + spec$coefficients[[nm]] * value
  }

  stats::plogis(eta)
}

calculate_expected_goals_ridge <- function(pbp) {
  pbp <- add_model_features(pbp)  # package-side mirror of models/xG/prepare.R feature engineering
  partition <- partition_shots(pbp)
  pbp$xg <- NA_real_
  for (key in names(partition)) {
    idx <- partition[[key]]
    pbp$xg[idx] <- score_ridge_partition(pbp[idx, , drop = FALSE], MODEL_SPECS[[key]])
  }
  pbp
}
```

## Current Training Summary

| dataset | partition | seasons | games | rows | goal_rate | best_penalty | cv_log_loss | cv_roc_auc | cv_pr_auc | cv_brier | search_note |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| sd | 5v5 Score/State-Adjusted | 2023,2024 | 2798 | 188930 | 0.0593235589901022 | 0.0000001 | 0.198578681650525 | 0.771783953243345 | 0.980124742617401 | 0.0525321777194838 | accepted_lower_boundary |
| ev | Other Even Strength | 2023,2024 | 1280 | 4907 | 0.111269614835949 | 0.0385662042116347 | 0.331397134078224 | 0.67283516277858 | 0.937637955445998 | 0.0953233729945728 | none |
| pp | Power Play | 2023,2024 | 2793 | 38903 | 0.0972932678713724 | 0.0000001 | 0.303556139340419 | 0.669264538566136 | 0.946342577289955 | 0.0851807617637989 | accepted_lower_boundary |
| sh | Short-Handed | 2023,2024 | 2241 | 5539 | 0.0738400433291208 | 0.0000001 | 0.221051990106459 | 0.796020292767421 | 0.980318920925609 | 0.0627852175246014 | accepted_lower_boundary |
| en | Empty Net Against | 2023,2024 | 1245 | 1828 | 0.573851203501094 | 0.0788046281566992 | 0.619050049619958 | 0.700180975003345 | 0.599215259469518 | 0.216064913970363 | none |
| so | Shootout / Penalty Shot | 2023,2024 | 230 | 1188 | 0.315656565656566 | 0.672335753649933 | 0.624135358640592 | 0.526419103398221 | 0.720480968075048 | 0.216268567879362 | none |

The `cv_*` columns above are grouped cross-validation means at the selected penalty. They are tuning diagnostics, not external `2025-26` test metrics.

## External Validation Summary

The compare-style external evaluation in [test.R](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/test.R) is now complete, and the generated outputs live in [results](/Users/rsai_91/Desktop/Work/rentosrink/models/xG/nhlscraper/results).

Overall results by evaluation season:

| season | model | rows | goals | total_xg | goal_rate | xg_rate | log_loss | brier | roc_auc | pr_auc | calibration_ratio | calibration_error |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2021-22 | ridge_glmnet | 122341 | 8936 | 9258.57878097065 | 0.0730438688541061 | 0.0756774251285842 | 0.231567980277077 | 0.0624904433611446 | 0.746283062034699 | 0.223554913221832 | 1.03632371877917 | 0.0363237187791708 |
| 2023-24 | ridge_glmnet | 122180 | 8771 | 8734.33625114591 | 0.0717881813717466 | 0.0714887566757717 | 0.222168683942953 | 0.0605159579505294 | 0.777506473295121 | 0.244399196362704 | 0.995822626171237 | 0.00417737382876272 |
| 2025-26 | ridge_glmnet | 74169 | 5521 | 5777.87174142816 | 0.0744354162822734 | 0.0778983594911791 | 0.231862123052462 | 0.0632027731918452 | 0.761722443358218 | 0.234128659171533 | 1.04652576624582 | 0.0465257662458153 |

True unseen-future `2025-26` results by partition:

| dataset | rows | goals | total_xg | goal_rate | xg_rate | log_loss | brier | roc_auc | pr_auc | calibration_ratio | calibration_error |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| en | 604 | 328 | 331.786717236475 | 0.543046357615894 | 0.549316750391515 | 0.595858199851634 | 0.206345636609383 | 0.740004591368228 | 0.77426727986404 | 1.01154486901364 | 0.0115448690136433 |
| ev | 1750 | 182 | 208.980988174324 | 0.104 | 0.119417707528185 | 0.310907739234827 | 0.0886110295363554 | 0.702086855080325 | 0.218766741138551 | 1.14824718777099 | 0.14824718777099 |
| pp | 12489 | 1192 | 1289.45469974658 | 0.0954439915125318 | 0.103246913987723 | 0.304484173590019 | 0.0844276919723858 | 0.651700396355764 | 0.155445907467866 | 1.08175729844512 | 0.0817572984451175 |
| sd | 57157 | 3523 | 3637.01678859239 | 0.0616379456514509 | 0.0636323538943026 | 0.205612745001144 | 0.0548317668556206 | 0.761534217696758 | 0.159162683238108 | 1.03236468057092 | 0.0323646805709239 |
| sh | 1610 | 112 | 132.577689963188 | 0.0695652173913043 | 0.0823463912814831 | 0.219823116893889 | 0.0624490083401214 | 0.784393238434164 | 0.164424527473264 | 1.18372937467132 | 0.183729374671323 |
| so | 559 | 184 | 177.055350493301 | 0.329159212880143 | 0.316736762957604 | 0.633589954411776 | 0.220856461776991 | 0.513107629992654 | 0.330673865549001 | 0.962257339637503 | 0.0377426603624969 |

## Partition Specs

## SD: 5v5 Score/State-Adjusted

- Raw predictor columns: `isPlayoff`, `isHome`, `isOvertime`, `periodNumber`, `secondsElapsedInPeriod`, `secondsElapsedInGame`, `secondsElapsedInSequence`, `zoneCode`, `xCoordNorm`, `yCoordNorm`, `dXCoordNorm`, `dYCoordNorm`, `distance`, `angle`, `dDistance`, `dAngle`, `dSecondsElapsedInSequence`, `dXCoordNormPerSecond`, `dYCoordNormPerSecond`, `dDistancePerSecond`, `dAnglePerSecond`, `isBehindNet`, `crossedRoyalRoad`, `typeDescKeyPrev`, `shotType`, `isRebound`, `isRush`, `goalsFor`, `goalsAgainst`, `goalDifferential`, `shotsFor`, `shotsAgainst`, `shotDifferential`, `fenwickFor`, `fenwickAgainst`, `fenwickDifferential`, `corsiFor`, `corsiAgainst`, `corsiDifferential`, `shooterHeight`, `shooterWeight`, `shooterHandCode`, `shooterPositionCode`, `shooterAge`, `shooterSecondsElapsedInShift`, `shooterSecondsElapsedSinceLastShift`, `goalieHeight`, `goalieWeight`, `goalieHandCode`, `goalieAge`, `minSecondsElapsedInShiftFor`, `maxSecondsElapsedInShiftFor`, `avgSecondsElapsedInShiftFor`, `minSecondsElapsedInShiftAgainst`, `maxSecondsElapsedInShiftAgainst`, `avgSecondsElapsedInShiftAgainst`, `minSecondsElapsedSinceLastShiftFor`, `maxSecondsElapsedSinceLastShiftFor`, `avgSecondsElapsedSinceLastShiftFor`, `minSecondsElapsedSinceLastShiftAgainst`, `maxSecondsElapsedSinceLastShiftAgainst`, `avgSecondsElapsedSinceLastShiftAgainst`
- Best penalty (`glmnet` lambda): `0.0000001`
- Final searched log10 range: `[-7, 2]`
- Search note: `accepted_lower_boundary`

Training summary:

| seasons | games | rows | goal_rate |
| --- | --- | --- | --- |
| 2023,2024 | 2798 | 188930 | 0.0593235589901022 |

Best-penalty grouped CV metrics:

| .metric | mean | std_err |
| --- | --- | --- |
| mn_log_loss | 0.198578681650525 | 0.00215759914113046 |
| roc_auc | 0.771783953243345 | 0.00218829095460852 |
| pr_auc | 0.980124742617401 | 0.000497852691532144 |
| brier_class | 0.0525321777194838 | 0.000670875410687333 |

Top grouped CV log-loss settings:

| penalty | .metric | .estimator | mean | n | std_err | .config |
| --- | --- | --- | --- | --- | --- | --- |
| 0.0000001 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod01_post0 |
| 0.000000204335971785694 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod02_post0 |
| 0.00000041753189365604 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod03_post0 |
| 0.000000853167852417281 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod04_post0 |
| 0.00000174332882219999 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod05_post0 |
| 0.00000356224789026244 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod06_post0 |
| 0.00000727895384398316 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod07_post0 |
| 0.0000148735210729351 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod08_post0 |
| 0.000030391953823132 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod09_post0 |
| 0.0000621016941891562 | mn_log_loss | binary | 0.198578681650525 | 5 | 0.00215759914113046 | pre0_mod10_post0 |

Penalty search history:

| iteration | log10_lower | log10_upper | penalty_lower | penalty_upper | best_penalty | best_cv_mn_log_loss | boundary_hit |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | -7 | 2 | 0.0000001 | 100 | 0.0000001 | 0.198578681650525 | accepted_lower_boundary |

Coefficients:

| term | estimate |
| --- | --- |
| (Intercept) | -3.21662396111928 |
| distance | -0.845270223348677 |
| angle | -0.440649801347895 |
| xCoordNorm | 0.382441853773176 |
| shotType_snap | 0.219261315443348 |
| shotType_slap | 0.151081613267939 |
| shotType_tip.in | -0.139544313823514 |
| dDistancePerSecond | 0.134718583670491 |
| dDistance | -0.133049324745222 |
| dAnglePerSecond | 0.123894122309696 |
| shotType_wrist | 0.115203292014181 |
| dXCoordNormPerSecond | -0.102720595424787 |
| dAngle | -0.0905423291332673 |
| avgSecondsElapsedInShiftFor | 0.0877470651015235 |
| minSecondsElapsedInShiftFor | 0.0810065618107748 |
| typeDescKeyPrev_giveaway.against | 0.0698001534338801 |
| crossedRoyalRoad_yes | 0.0688946390245284 |
| zoneCode_N | -0.0603786771240883 |
| shotType_deflected | -0.0436951550454854 |
| typeDescKeyPrev_takeaway.for | 0.0421174155384111 |
| typeDescKeyPrev_given.hit | -0.0409218537432748 |
| zoneCode_O | 0.0388806669373406 |
| typeDescKeyPrev_takeaway.against | -0.0378787468337453 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0.0356345887815009 |
| shooterSecondsElapsedSinceLastShift | -0.0354509751521583 |
| typeDescKeyPrev_post.missed.shot.for | 0.0341140139623162 |
| avgSecondsElapsedInShiftAgainst | 0.0337916539642249 |
| typeDescKeyPrev_won.faceoff | -0.0336037573497317 |
| shooterPositionCode_D | -0.0313785326365604 |
| typeDescKeyPrev_lost.faceoff | -0.0307375070025599 |
| typeDescKeyPrev_taken.hit | -0.0283054851576127 |
| fenwickFor | -0.0278322144448358 |
| goalDifferential | 0.0271196029500246 |
| shooterSecondsElapsedInShift | -0.0263817136158556 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0.0263732013874639 |
| goalsFor | 0.0250340616001581 |
| maxSecondsElapsedSinceLastShiftAgainst | 0.0249731812809963 |
| secondsElapsedInSequence | 0.0247540875769382 |
| typeDescKeyPrev_wide.missed.shot.against | 0.0247507271841103 |
| shooterAge | -0.0239061975276608 |
| fenwickDifferential | -0.0237104337028808 |
| goalieAge | -0.022496194634576 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0.0214997031316908 |
| typeDescKeyPrev_post.missed.shot.against | -0.0204416595326926 |
| minSecondsElapsedInShiftAgainst | 0.0198312521309 |
| dSecondsElapsedInSequence | -0.0188030652809314 |
| goalieWeight | -0.0184659712000265 |
| dYCoordNormPerSecond | 0.0184314685917693 |
| typeDescKeyPrev_blocked.shot.against | 0.0178706897830129 |
| isRebound_yes | -0.0171926071535487 |
| shooterHandCode_R | 0.0158820290706298 |
| typeDescKeyPrev_blocked.shot.for | -0.0145587841285273 |
| shooterWeight | 0.0139823026653148 |
| typeDescKeyPrev_other.shot.on.goal.against | 0.0138516046534425 |
| secondsElapsedInPeriod | -0.0136721241084252 |
| periodNumber | 0.013608165242836 |
| corsiDifferential | 0.0133225411674586 |
| minSecondsElapsedSinceLastShiftAgainst | -0.0116171304495617 |
| minSecondsElapsedSinceLastShiftFor | 0.0115767150571529 |
| goalieHandCode_R | -0.0112995532379208 |
| shooterHeight | -0.0112876726102423 |
| typeDescKeyPrev_other.missed.shot.against | 0.0109316584385817 |
| fenwickAgainst | -0.0109278076807191 |
| typeDescKeyPrev_giveaway.for | 0.0108623755513644 |
| dYCoordNorm | -0.0107718201697977 |
| typeDescKeyPrev_snap.shot.on.goal.against | -0.0107404182517506 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0.0105837885430289 |
| shooterPositionCode_R | 0.0105409505047211 |
| avgSecondsElapsedSinceLastShiftFor | -0.00975747016438907 |
| typeDescKeyPrev_other.shot.on.goal.for | 0.00964595522286215 |
| typeDescKeyPrev_backhand.shot.on.goal.for | -0.0087902285788831 |
| shooterPositionCode_L | 0.00860552307642466 |
| typeDescKeyPrev_high.missed.shot.for | 0.00833315853909945 |
| secondsElapsedInGame | 0.00799184219209882 |
| isHome_yes | 0.00784694830873321 |
| goalsAgainst | -0.00704098820119407 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0.0066153550161316 |
| maxSecondsElapsedInShiftAgainst | -0.00659107444052173 |
| isRush_yes | 0.00652523435241672 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0.00650608692144187 |
| typeDescKeyPrev_high.missed.shot.against | 0.0065047919815252 |
| corsiAgainst | -0.00626893877823523 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0.00550696114855524 |
| maxSecondsElapsedInShiftFor | 0.00365810831796548 |
| typeDescKeyPrev_wide.missed.shot.for | -0.00332024781872199 |
| dXCoordNorm | -0.00300845620873193 |
| isBehindNet_yes | -0.00297281752499956 |
| corsiFor | 0.00292897584161385 |
| isPlayoff_yes | -0.0025790338772304 |
| shotsFor | 0.0025413643875168 |
| typeDescKeyPrev_deflected.shot.on.goal.for | 0.00230656970555288 |
| yCoordNorm | 0.00210338831405524 |
| shotDifferential | 0.00205953020832411 |
| typeDescKeyPrev_slap.shot.on.goal.against | -0.00163730399000494 |
| shotType_other | -0.00162556084087442 |
| isOvertime_yes | 0.00145476926351948 |
| goalieHeight | 0.0012919760822313 |
| avgSecondsElapsedSinceLastShiftAgainst | -0.000787413257790284 |
| shotsAgainst | 0.000666846203571509 |
| maxSecondsElapsedSinceLastShiftFor | 0.000138864005844226 |
| typeDescKeyPrev_other.missed.shot.for | 0.000109200575500076 |

Numeric imputation medians:

| term | median |
| --- | --- |
| periodNumber | 2 |
| secondsElapsedInPeriod | 574 |
| secondsElapsedInGame | 1736 |
| secondsElapsedInSequence | 47 |
| xCoordNorm | 64 |
| yCoordNorm | 0 |
| dXCoordNorm | 12 |
| dYCoordNorm | 0 |
| distance | 33.8378486313773 |
| angle | 31.9208762199332 |
| dDistance | -15.0372394578359 |
| dAngle | 4.56924244025109 |
| dSecondsElapsedInSequence | 10 |
| dXCoordNormPerSecond | 1.17391304347826 |
| dYCoordNormPerSecond | 0 |
| dDistancePerSecond | -1.35210872547523 |
| dAnglePerSecond | 0.30601883059752 |
| goalsFor | 1 |
| goalsAgainst | 1 |
| goalDifferential | 0 |
| shotsFor | 13 |
| shotsAgainst | 13 |
| shotDifferential | 0 |
| fenwickFor | 20 |
| fenwickAgainst | 19 |
| fenwickDifferential | 0 |
| corsiFor | 28 |
| corsiAgainst | 27 |
| corsiDifferential | 1 |
| shooterHeight | 73 |
| shooterWeight | 201 |
| shooterAge | 28 |
| shooterSecondsElapsedInShift | 26 |
| shooterSecondsElapsedSinceLastShift | 164 |
| goalieHeight | 75 |
| goalieWeight | 201 |
| goalieAge | 28 |
| minSecondsElapsedInShiftFor | 13 |
| maxSecondsElapsedInShiftFor | 38 |
| avgSecondsElapsedInShiftFor | 26.2 |
| minSecondsElapsedInShiftAgainst | 18 |
| maxSecondsElapsedInShiftAgainst | 43 |
| avgSecondsElapsedInShiftAgainst | 31 |
| minSecondsElapsedSinceLastShiftFor | 105 |
| maxSecondsElapsedSinceLastShiftFor | 214 |
| avgSecondsElapsedSinceLastShiftFor | 163.6 |
| minSecondsElapsedSinceLastShiftAgainst | 112 |
| maxSecondsElapsedSinceLastShiftAgainst | 221 |
| avgSecondsElapsedSinceLastShiftAgainst | 169.6 |
| isPlayoff_yes | 0 |
| isPlayoff_unknown | 0 |
| isPlayoff_new | 0 |
| isHome_yes | 1 |
| isHome_unknown | 0 |
| isHome_new | 0 |
| isOvertime_yes | 0 |
| isOvertime_unknown | 0 |
| isOvertime_new | 0 |
| zoneCode_N | 0 |
| zoneCode_O | 1 |
| zoneCode_unknown | 0 |
| zoneCode_new | 0 |
| isBehindNet_yes | 0 |
| isBehindNet_unknown | 0 |
| isBehindNet_new | 0 |
| crossedRoyalRoad_yes | 0 |
| crossedRoyalRoad_unknown | 0 |
| crossedRoyalRoad_new | 0 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0 |
| typeDescKeyPrev_blocked.shot.against | 0 |
| typeDescKeyPrev_blocked.shot.for | 0 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0 |
| typeDescKeyPrev_deflected.shot.on.goal.for | 0 |
| typeDescKeyPrev_giveaway.against | 0 |
| typeDescKeyPrev_giveaway.for | 0 |
| typeDescKeyPrev_given.hit | 0 |
| typeDescKeyPrev_high.missed.shot.against | 0 |
| typeDescKeyPrev_high.missed.shot.for | 0 |
| typeDescKeyPrev_lost.faceoff | 0 |
| typeDescKeyPrev_other.missed.shot.against | 0 |
| typeDescKeyPrev_other.missed.shot.for | 0 |
| typeDescKeyPrev_other.shot.on.goal.against | 0 |
| typeDescKeyPrev_other.shot.on.goal.for | 0 |
| typeDescKeyPrev_post.missed.shot.against | 0 |
| typeDescKeyPrev_post.missed.shot.for | 0 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0 |
| typeDescKeyPrev_takeaway.against | 0 |
| typeDescKeyPrev_takeaway.for | 0 |
| typeDescKeyPrev_taken.hit | 0 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0 |
| typeDescKeyPrev_wide.missed.shot.against | 0 |
| typeDescKeyPrev_wide.missed.shot.for | 0 |
| typeDescKeyPrev_won.faceoff | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0 |
| typeDescKeyPrev_unknown | 0 |
| typeDescKeyPrev_new | 0 |
| shotType_deflected | 0 |
| shotType_other | 0 |
| shotType_slap | 0 |
| shotType_snap | 0 |
| shotType_tip.in | 0 |
| shotType_wrist | 1 |
| shotType_unknown | 0 |
| shotType_new | 0 |
| isRebound_yes | 0 |
| isRebound_unknown | 0 |
| isRebound_new | 0 |
| isRush_yes | 0 |
| isRush_unknown | 0 |
| isRush_new | 0 |
| shooterHandCode_R | 0 |
| shooterHandCode_unknown | 0 |
| shooterHandCode_new | 0 |
| shooterPositionCode_D | 0 |
| shooterPositionCode_L | 0 |
| shooterPositionCode_R | 0 |
| shooterPositionCode_unknown | 0 |
| shooterPositionCode_new | 0 |
| goalieHandCode_R | 0 |
| goalieHandCode_unknown | 0 |
| goalieHandCode_new | 0 |

Numeric normalization parameters:

| term | mean | sd |
| --- | --- | --- |
| periodNumber | 1.97593817816122 | 0.815367666258036 |
| secondsElapsedInPeriod | 580.931800137617 | 342.145874812043 |
| secondsElapsedInGame | 1752.05761393109 | 1027.16930003502 |
| secondsElapsedInSequence | 65.9353464246017 | 62.0182173980348 |
| xCoordNorm | 59.2784311649817 | 23.7493578502867 |
| yCoordNorm | 0.565156407134918 | 20.9211134369813 |
| dXCoordNorm | 32.2800825702641 | 63.6671090197149 |
| dYCoordNorm | 0.205144762610491 | 31.608245170542 |
| distance | 36.2887392112754 | 23.8441010556452 |
| angle | 34.3816865703314 | 22.5597460466179 |
| dDistance | -32.9776066258113 | 60.3256939058129 |
| dAngle | 1.46145965757483 | 37.7484175343664 |
| dSecondsElapsedInSequence | 14.4975334780077 | 14.6283483101272 |
| dXCoordNormPerSecond | 7.02538530143823 | 27.9067683694323 |
| dYCoordNormPerSecond | 0.116477849625709 | 11.3811694622014 |
| dDistancePerSecond | -7.21182892466394 | 26.6539463664115 |
| dAnglePerSecond | 0.420884062773812 | 14.0882523541643 |
| goalsFor | 1.29279098078653 | 1.37987782178084 |
| goalsAgainst | 1.38179749113428 | 1.44204155126493 |
| goalDifferential | -0.0890065103477478 | 1.64570880163997 |
| shotsFor | 14.1953104324353 | 9.46022899155838 |
| shotsAgainst | 13.8557825649712 | 9.32729691380714 |
| shotDifferential | 0.33952786746414 | 6.72027013899804 |
| fenwickFor | 20.8965119356375 | 13.6017089771962 |
| fenwickAgainst | 20.2808659291801 | 13.3194312534501 |
| fenwickDifferential | 0.615646006457418 | 9.18412838249186 |
| corsiFor | 29.2364949981475 | 18.7659758617613 |
| corsiAgainst | 28.3003493357328 | 18.3116476570591 |
| corsiDifferential | 0.936145662414651 | 12.3104785152423 |
| shooterHeight | 73.2679669718944 | 2.16033751679746 |
| shooterWeight | 201.215698936114 | 15.498575194985 |
| shooterAge | 27.7459111840364 | 4.17687267586859 |
| shooterSecondsElapsedInShift | 28.661885354364 | 18.0895270113673 |
| shooterSecondsElapsedSinceLastShift | 195.443174720796 | 114.168639825015 |
| goalieHeight | 74.9193457894458 | 1.61621488341152 |
| goalieWeight | 201.733980839464 | 15.6285478511863 |
| goalieAge | 28.6075265971524 | 3.73725825515807 |
| minSecondsElapsedInShiftFor | 15.8918911766263 | 13.3006443103212 |
| maxSecondsElapsedInShiftFor | 39.5768221034245 | 20.3711910737462 |
| avgSecondsElapsedInShiftFor | 27.3375292436355 | 13.3808016846489 |
| minSecondsElapsedInShiftAgainst | 21.2371672047848 | 15.6677341653142 |
| maxSecondsElapsedInShiftAgainst | 45.2782564971153 | 24.3228766990332 |
| avgSecondsElapsedInShiftAgainst | 32.5625833201006 | 16.7831486282317 |
| minSecondsElapsedSinceLastShiftFor | 129.91762557561 | 93.5788593175964 |
| maxSecondsElapsedSinceLastShiftFor | 252.625697348224 | 121.83047221752 |
| avgSecondsElapsedSinceLastShiftFor | 189.98623528997 | 88.348610818704 |
| minSecondsElapsedSinceLastShiftAgainst | 137.968183983486 | 96.4868762543344 |
| maxSecondsElapsedSinceLastShiftAgainst | 259.200201132695 | 123.413777106872 |
| avgSecondsElapsedSinceLastShiftAgainst | 196.319421478856 | 90.4813093150859 |
| isPlayoff_yes | 0.0636796697189435 | 0.244182073412473 |
| isHome_yes | 0.50817233896152 | 0.499934531484392 |
| isOvertime_yes | 0.00317577938919176 | 0.0562646476080687 |
| zoneCode_N | 0.0290636743767533 | 0.167985495121614 |
| zoneCode_O | 0.960477425501508 | 0.194835165020531 |
| isBehindNet_yes | 0.0211930344572064 | 0.14402777351929 |
| crossedRoyalRoad_yes | 0.441523315513682 | 0.496570017741822 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0.0106917906102789 | 0.102847130297313 |
| typeDescKeyPrev_blocked.shot.against | 0.0399089609908432 | 0.195745852142385 |
| typeDescKeyPrev_blocked.shot.for | 0.096279045148997 | 0.294974322872501 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0.000725136293865453 | 0.0269186609358503 |
| typeDescKeyPrev_deflected.shot.on.goal.for | 0.00233949081670459 | 0.0483117993182744 |
| typeDescKeyPrev_giveaway.against | 0.0792992113481183 | 0.270205908287975 |
| typeDescKeyPrev_giveaway.for | 0.0284708622241042 | 0.166314216572009 |
| typeDescKeyPrev_given.hit | 0.0871222145768274 | 0.282014813912443 |
| typeDescKeyPrev_high.missed.shot.against | 0.00190546763351505 | 0.0436101696056292 |
| typeDescKeyPrev_high.missed.shot.for | 0.00421849362197639 | 0.0648129629612354 |
| typeDescKeyPrev_lost.faceoff | 0.0724183560048695 | 0.259180040260499 |
| typeDescKeyPrev_other.missed.shot.against | 0.00144497962208225 | 0.0379855142546053 |
| typeDescKeyPrev_other.missed.shot.for | 0.00282115069073201 | 0.0530396709050628 |
| typeDescKeyPrev_other.shot.on.goal.against | 0.000524003599216641 | 0.0228851871641829 |
| typeDescKeyPrev_other.shot.on.goal.for | 0.00286349441592124 | 0.0534351001561588 |
| typeDescKeyPrev_post.missed.shot.against | 0.00191076059916371 | 0.0436705814870398 |
| typeDescKeyPrev_post.missed.shot.for | 0.0059492933890859 | 0.0769020844936986 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0.00439845445403059 | 0.0661750045790696 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0.0118827078812259 | 0.108358531193356 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0.00774360874397925 | 0.0876566365831477 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0.0215159053617742 | 0.145096804277118 |
| typeDescKeyPrev_takeaway.against | 0.0147038585719579 | 0.120364994072822 |
| typeDescKeyPrev_takeaway.for | 0.0402794685862489 | 0.196614438961235 |
| typeDescKeyPrev_taken.hit | 0.114285714285714 | 0.318158805593698 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0.00302757635102948 | 0.0549402048493536 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0.0099984121103054 | 0.0994911868357685 |
| typeDescKeyPrev_wide.missed.shot.against | 0.0243211771555603 | 0.154044743819163 |
| typeDescKeyPrev_wide.missed.shot.for | 0.06836923728365 | 0.252379123172931 |
| typeDescKeyPrev_won.faceoff | 0.146255226803578 | 0.353362839506001 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0.0227597522892076 | 0.149137063436473 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0.0686180066691367 | 0.252804102226798 |
| shotType_deflected | 0.018758270258826 | 0.135670538366381 |
| shotType_other | 0.0170909860795003 | 0.129610852905064 |
| shotType_slap | 0.103943259408246 | 0.305187731103722 |
| shotType_snap | 0.174752553855925 | 0.379756319368894 |
| shotType_tip.in | 0.0875456518287196 | 0.282633744423366 |
| shotType_wrist | 0.522622135182343 | 0.49948929872391 |
| isRebound_yes | 0.101206796167893 | 0.301603153247352 |
| isRush_yes | 0.075202455936061 | 0.263718438243215 |
| shooterHandCode_R | 0.375086010691791 | 0.484146399278003 |
| shooterPositionCode_D | 0.303424548774678 | 0.459738197991455 |
| shooterPositionCode_L | 0.202011326946488 | 0.401501686144252 |
| shooterPositionCode_R | 0.174059175355952 | 0.37916136375457 |
| goalieHandCode_R | 0.0530090509712592 | 0.224052130518436 |

Dummy map:

| variable | level | output_column |
| --- | --- | --- |
| isPlayoff | yes | isPlayoff_yes |
| isPlayoff | unknown | isPlayoff_unknown |
| isPlayoff | new | isPlayoff_new |
| isHome | yes | isHome_yes |
| isHome | unknown | isHome_unknown |
| isHome | new | isHome_new |
| isOvertime | yes | isOvertime_yes |
| isOvertime | unknown | isOvertime_unknown |
| isOvertime | new | isOvertime_new |
| zoneCode | N | zoneCode_N |
| zoneCode | O | zoneCode_O |
| zoneCode | unknown | zoneCode_unknown |
| zoneCode | new | zoneCode_new |
| isBehindNet | yes | isBehindNet_yes |
| isBehindNet | unknown | isBehindNet_unknown |
| isBehindNet | new | isBehindNet_new |
| crossedRoyalRoad | yes | crossedRoyalRoad_yes |
| crossedRoyalRoad | unknown | crossedRoyalRoad_unknown |
| crossedRoyalRoad | new | crossedRoyalRoad_new |
| typeDescKeyPrev | backhand-shot-on-goal-for | typeDescKeyPrev_backhand.shot.on.goal.for |
| typeDescKeyPrev | blocked-shot-against | typeDescKeyPrev_blocked.shot.against |
| typeDescKeyPrev | blocked-shot-for | typeDescKeyPrev_blocked.shot.for |
| typeDescKeyPrev | deflected-shot-on-goal-against | typeDescKeyPrev_deflected.shot.on.goal.against |
| typeDescKeyPrev | deflected-shot-on-goal-for | typeDescKeyPrev_deflected.shot.on.goal.for |
| typeDescKeyPrev | giveaway-against | typeDescKeyPrev_giveaway.against |
| typeDescKeyPrev | giveaway-for | typeDescKeyPrev_giveaway.for |
| typeDescKeyPrev | given-hit | typeDescKeyPrev_given.hit |
| typeDescKeyPrev | high-missed-shot-against | typeDescKeyPrev_high.missed.shot.against |
| typeDescKeyPrev | high-missed-shot-for | typeDescKeyPrev_high.missed.shot.for |
| typeDescKeyPrev | lost-faceoff | typeDescKeyPrev_lost.faceoff |
| typeDescKeyPrev | other-missed-shot-against | typeDescKeyPrev_other.missed.shot.against |
| typeDescKeyPrev | other-missed-shot-for | typeDescKeyPrev_other.missed.shot.for |
| typeDescKeyPrev | other-shot-on-goal-against | typeDescKeyPrev_other.shot.on.goal.against |
| typeDescKeyPrev | other-shot-on-goal-for | typeDescKeyPrev_other.shot.on.goal.for |
| typeDescKeyPrev | post-missed-shot-against | typeDescKeyPrev_post.missed.shot.against |
| typeDescKeyPrev | post-missed-shot-for | typeDescKeyPrev_post.missed.shot.for |
| typeDescKeyPrev | slap-shot-on-goal-against | typeDescKeyPrev_slap.shot.on.goal.against |
| typeDescKeyPrev | slap-shot-on-goal-for | typeDescKeyPrev_slap.shot.on.goal.for |
| typeDescKeyPrev | snap-shot-on-goal-against | typeDescKeyPrev_snap.shot.on.goal.against |
| typeDescKeyPrev | snap-shot-on-goal-for | typeDescKeyPrev_snap.shot.on.goal.for |
| typeDescKeyPrev | takeaway-against | typeDescKeyPrev_takeaway.against |
| typeDescKeyPrev | takeaway-for | typeDescKeyPrev_takeaway.for |
| typeDescKeyPrev | taken-hit | typeDescKeyPrev_taken.hit |
| typeDescKeyPrev | tip-in-shot-on-goal-against | typeDescKeyPrev_tip.in.shot.on.goal.against |
| typeDescKeyPrev | tip-in-shot-on-goal-for | typeDescKeyPrev_tip.in.shot.on.goal.for |
| typeDescKeyPrev | wide-missed-shot-against | typeDescKeyPrev_wide.missed.shot.against |
| typeDescKeyPrev | wide-missed-shot-for | typeDescKeyPrev_wide.missed.shot.for |
| typeDescKeyPrev | won-faceoff | typeDescKeyPrev_won.faceoff |
| typeDescKeyPrev | wrist-shot-on-goal-against | typeDescKeyPrev_wrist.shot.on.goal.against |
| typeDescKeyPrev | wrist-shot-on-goal-for | typeDescKeyPrev_wrist.shot.on.goal.for |
| typeDescKeyPrev | unknown | typeDescKeyPrev_unknown |
| typeDescKeyPrev | new | typeDescKeyPrev_new |
| shotType | deflected | shotType_deflected |
| shotType | other | shotType_other |
| shotType | slap | shotType_slap |
| shotType | snap | shotType_snap |
| shotType | tip-in | shotType_tip.in |
| shotType | wrist | shotType_wrist |
| shotType | unknown | shotType_unknown |
| shotType | new | shotType_new |
| isRebound | yes | isRebound_yes |
| isRebound | unknown | isRebound_unknown |
| isRebound | new | isRebound_new |
| isRush | yes | isRush_yes |
| isRush | unknown | isRush_unknown |
| isRush | new | isRush_new |
| shooterHandCode | R | shooterHandCode_R |
| shooterHandCode | unknown | shooterHandCode_unknown |
| shooterHandCode | new | shooterHandCode_new |
| shooterPositionCode | D | shooterPositionCode_D |
| shooterPositionCode | L | shooterPositionCode_L |
| shooterPositionCode | R | shooterPositionCode_R |
| shooterPositionCode | unknown | shooterPositionCode_unknown |
| shooterPositionCode | new | shooterPositionCode_new |
| goalieHandCode | R | goalieHandCode_R |
| goalieHandCode | unknown | goalieHandCode_unknown |
| goalieHandCode | new | goalieHandCode_new |

Unknown categorical fills:

| variable | value |
| --- | --- |
| isPlayoff | unknown |
| isHome | unknown |
| isOvertime | unknown |
| zoneCode | unknown |
| isBehindNet | unknown |
| crossedRoyalRoad | unknown |
| typeDescKeyPrev | unknown |
| shotType | unknown |
| isRebound | unknown |
| isRush | unknown |
| shooterHandCode | unknown |
| shooterPositionCode | unknown |
| goalieHandCode | unknown |

Novel categorical buckets:

| variable | value |
| --- | --- |
| isPlayoff | new |
| isHome | new |
| isOvertime | new |
| zoneCode | new |
| isBehindNet | new |
| crossedRoyalRoad | new |
| typeDescKeyPrev | new |
| shotType | new |
| isRebound | new |
| isRush | new |
| shooterHandCode | new |
| shooterPositionCode | new |
| goalieHandCode | new |

Zero-variance terms removed before fitting:

| term |
| --- |
| isPlayoff_unknown |
| isPlayoff_new |
| isHome_unknown |
| isHome_new |
| isOvertime_unknown |
| isOvertime_new |
| zoneCode_unknown |
| zoneCode_new |
| isBehindNet_unknown |
| isBehindNet_new |
| crossedRoyalRoad_unknown |
| crossedRoyalRoad_new |
| typeDescKeyPrev_unknown |
| typeDescKeyPrev_new |
| shotType_unknown |
| shotType_new |
| isRebound_unknown |
| isRebound_new |
| isRush_unknown |
| isRush_new |
| shooterHandCode_unknown |
| shooterHandCode_new |
| shooterPositionCode_unknown |
| shooterPositionCode_new |
| goalieHandCode_unknown |
| goalieHandCode_new |

## EV: Other Even Strength

- Raw predictor columns: `isPlayoff`, `isHome`, `isOvertime`, `periodNumber`, `secondsElapsedInPeriod`, `secondsElapsedInGame`, `secondsElapsedInSequence`, `zoneCode`, `xCoordNorm`, `yCoordNorm`, `dXCoordNorm`, `dYCoordNorm`, `distance`, `angle`, `dDistance`, `dAngle`, `dSecondsElapsedInSequence`, `dXCoordNormPerSecond`, `dYCoordNormPerSecond`, `dDistancePerSecond`, `dAnglePerSecond`, `isBehindNet`, `crossedRoyalRoad`, `typeDescKeyPrev`, `shotType`, `isRebound`, `isRush`, `goalsFor`, `goalsAgainst`, `goalDifferential`, `shotsFor`, `shotsAgainst`, `shotDifferential`, `fenwickFor`, `fenwickAgainst`, `fenwickDifferential`, `corsiFor`, `corsiAgainst`, `corsiDifferential`, `shooterHeight`, `shooterWeight`, `shooterHandCode`, `shooterPositionCode`, `shooterAge`, `shooterSecondsElapsedInShift`, `shooterSecondsElapsedSinceLastShift`, `goalieHeight`, `goalieWeight`, `goalieHandCode`, `goalieAge`, `minSecondsElapsedInShiftFor`, `maxSecondsElapsedInShiftFor`, `avgSecondsElapsedInShiftFor`, `minSecondsElapsedInShiftAgainst`, `maxSecondsElapsedInShiftAgainst`, `avgSecondsElapsedInShiftAgainst`, `minSecondsElapsedSinceLastShiftFor`, `maxSecondsElapsedSinceLastShiftFor`, `avgSecondsElapsedSinceLastShiftFor`, `minSecondsElapsedSinceLastShiftAgainst`, `maxSecondsElapsedSinceLastShiftAgainst`, `avgSecondsElapsedSinceLastShiftAgainst`, `isEmptyNetFor`, `skaterCountFor`, `skaterCountAgainst`, `manDifferential`, `strengthState`
- Best penalty (`glmnet` lambda): `0.0385662042116347`
- Final searched log10 range: `[-7, 2]`
- Search note: `none`

Training summary:

| seasons | games | rows | goal_rate |
| --- | --- | --- | --- |
| 2023,2024 | 1280 | 4907 | 0.111269614835949 |

Best-penalty grouped CV metrics:

| .metric | mean | std_err |
| --- | --- | --- |
| mn_log_loss | 0.331397134078224 | 0.00646040549454942 |
| roc_auc | 0.67283516277858 | 0.00796329444638462 |
| pr_auc | 0.937637955445998 | 0.00294470994604145 |
| brier_class | 0.0953233729945728 | 0.0021713308340679 |

Top grouped CV log-loss settings:

| penalty | .metric | .estimator | mean | n | std_err | .config |
| --- | --- | --- | --- | --- | --- | --- |
| 0.0385662042116347 | mn_log_loss | binary | 0.331397134078224 | 5 | 0.00646040549454942 | pre0_mod19_post0 |
| 0.018873918221351 | mn_log_loss | binary | 0.331711705249158 | 5 | 0.00639906799918281 | pre0_mod18_post0 |
| 0.0788046281566992 | mn_log_loss | binary | 0.332105327974841 | 5 | 0.00643229782721779 | pre0_mod20_post0 |
| 0.00923670857187386 | mn_log_loss | binary | 0.332739285609451 | 5 | 0.00624654859944088 | pre0_mod17_post0 |
| 0.0000001 | mn_log_loss | binary | 0.333694220898466 | 5 | 0.00611351105017692 | pre0_mod01_post0 |
| 0.000000204335971785694 | mn_log_loss | binary | 0.333694220898466 | 5 | 0.00611351105017692 | pre0_mod02_post0 |
| 0.00000041753189365604 | mn_log_loss | binary | 0.333694220898466 | 5 | 0.00611351105017692 | pre0_mod03_post0 |
| 0.000000853167852417281 | mn_log_loss | binary | 0.333694220898466 | 5 | 0.00611351105017692 | pre0_mod04_post0 |
| 0.00000174332882219999 | mn_log_loss | binary | 0.333694220898466 | 5 | 0.00611351105017692 | pre0_mod05_post0 |
| 0.00000356224789026244 | mn_log_loss | binary | 0.333694220898466 | 5 | 0.00611351105017692 | pre0_mod06_post0 |

Penalty search history:

| iteration | log10_lower | log10_upper | penalty_lower | penalty_upper | best_penalty | best_cv_mn_log_loss | boundary_hit |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | -7 | 2 | 0.0000001 | 100 | 0.0385662042116347 | 0.331397134078224 | none |

Coefficients:

| term | estimate |
| --- | --- |
| (Intercept) | -2.24423488073145 |
| distance | -0.347608860551493 |
| xCoordNorm | 0.230283359824653 |
| angle | -0.176081891251424 |
| shotType_snap | 0.148368045027988 |
| avgSecondsElapsedInShiftFor | 0.0899546633364154 |
| minSecondsElapsedInShiftFor | 0.0720465768768434 |
| dAngle | -0.0690286944708205 |
| dSecondsElapsedInSequence | -0.0678088759911804 |
| typeDescKeyPrev_blocked.shot.for | -0.0664219663709879 |
| dYCoordNorm | -0.0636410511425574 |
| typeDescKeyPrev_blocked.shot.against | 0.0629358641845923 |
| skaterCountAgainst | -0.0529157899083863 |
| skaterCountFor | -0.0528665349526811 |
| typeDescKeyPrev_giveaway.for | -0.0499855577200566 |
| maxSecondsElapsedSinceLastShiftAgainst | -0.048885358370869 |
| avgSecondsElapsedInShiftAgainst | 0.0488080106285123 |
| shooterPositionCode_D | -0.0473703477427517 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0.0470629331950644 |
| goalieHeight | -0.046308330690822 |
| isOvertime_yes | 0.0449094493025882 |
| shotType_slap | 0.0422791723147393 |
| shotType_other | 0.0417086920611954 |
| typeDescKeyPrev_wide.missed.shot.for | 0.0384888980219932 |
| minSecondsElapsedInShiftAgainst | -0.038299496753113 |
| shooterAge | 0.0376678164727882 |
| yCoordNorm | 0.0374926212248895 |
| typeDescKeyPrev_other.missed.shot.for | 0.0368965461217797 |
| shooterHeight | -0.0351637574524932 |
| typeDescKeyPrev_slap.shot.on.goal.for | -0.03395659759939 |
| goalDifferential | 0.0339559968934885 |
| periodNumber | 0.0326140380307751 |
| shooterWeight | 0.0321220478981391 |
| secondsElapsedInPeriod | -0.0317706666758868 |
| isPlayoff_yes | 0.0317239327454008 |
| typeDescKeyPrev_post.missed.shot.against | 0.0316081880844816 |
| typeDescKeyPrev_other.shot.on.goal.against | -0.0314261652756363 |
| goalieAge | -0.0310583058827592 |
| typeDescKeyPrev_other.missed.shot.against | -0.0306112261242234 |
| isRebound_yes | 0.0300093524101173 |
| secondsElapsedInSequence | 0.029575516407136 |
| typeDescKeyPrev_taken.hit | -0.0294310201599349 |
| secondsElapsedInGame | 0.0287642313404806 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0.0286061653078851 |
| shooterSecondsElapsedInShift | -0.0280028954020742 |
| shooterHandCode_R | -0.0273979400893407 |
| typeDescKeyPrev_wrist.shot.on.goal.for | -0.026932341618411 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0.0266583109446109 |
| dAnglePerSecond | 0.0243186690116709 |
| shooterPositionCode_R | -0.0218460540356297 |
| typeDescKeyPrev_high.missed.shot.for | -0.0209815358944664 |
| typeDescKeyPrev_high.missed.shot.against | 0.0204354636061654 |
| manDifferential | 0.0200483975472999 |
| isEmptyNetFor_yes | -0.0200440222347383 |
| strengthState_penalty.kill | -0.020039136516601 |
| zoneCode_N | 0.0184820948596959 |
| dDistance | -0.0182324587865813 |
| crossedRoyalRoad_yes | 0.0180983759403317 |
| goalsAgainst | -0.0177715841286841 |
| shotDifferential | -0.0173686124627538 |
| goalieWeight | -0.0166141367872776 |
| avgSecondsElapsedSinceLastShiftFor | -0.016572887704182 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | -0.0162020646971945 |
| isRush_yes | 0.0150564848823172 |
| shooterSecondsElapsedSinceLastShift | 0.0147477858599623 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0.0146682482485894 |
| goalsFor | 0.0142676029880193 |
| zoneCode_O | -0.0138943477965367 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0.0134420725230423 |
| maxSecondsElapsedSinceLastShiftFor | -0.0132179358271216 |
| dYCoordNormPerSecond | 0.0123605301092327 |
| dDistancePerSecond | 0.0121675783451817 |
| typeDescKeyPrev_takeaway.for | 0.0116549869717634 |
| isBehindNet_yes | 0.0114814876497155 |
| typeDescKeyPrev_lost.faceoff | -0.0113590346313562 |
| minSecondsElapsedSinceLastShiftAgainst | 0.0112072767106293 |
| typeDescKeyPrev_other.shot.on.goal.for | -0.0109682883796724 |
| avgSecondsElapsedSinceLastShiftAgainst | 0.0108506561441666 |
| shotType_tip.in | -0.010849812956661 |
| shooterPositionCode_L | 0.0102440729043912 |
| maxSecondsElapsedInShiftAgainst | 0.010216120814566 |
| shotsFor | -0.0100328545047931 |
| shotType_wrist | -0.00983184823235613 |
| typeDescKeyPrev_takeaway.against | -0.00922241802693485 |
| typeDescKeyPrev_deflected.shot.on.goal.for | -0.00912347553825898 |
| maxSecondsElapsedInShiftFor | 0.00831025489781561 |
| shotType_deflected | -0.0083020614638022 |
| typeDescKeyPrev_giveaway.against | 0.00773382620492336 |
| typeDescKeyPrev_post.missed.shot.for | 0.00767824644680835 |
| typeDescKeyPrev_wide.missed.shot.against | -0.00753292945873706 |
| typeDescKeyPrev_given.hit | -0.00741171030087287 |
| corsiAgainst | 0.00690809822804989 |
| goalieHandCode_R | 0.00648343873809993 |
| typeDescKeyPrev_snap.shot.on.goal.against | -0.0064164341658698 |
| corsiDifferential | -0.00570744684191754 |
| shotsAgainst | 0.00390875744388435 |
| typeDescKeyPrev_won.faceoff | 0.00281908804387055 |
| fenwickFor | 0.00247948260968648 |
| corsiFor | 0.00203684195222828 |
| fenwickAgainst | 0.00183171865169034 |
| isHome_yes | 0.00179644854015015 |
| dXCoordNorm | -0.00157874961816207 |
| dXCoordNormPerSecond | 0.00155559921511842 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0.000952283378117877 |
| fenwickDifferential | 0.000911119548418619 |
| minSecondsElapsedSinceLastShiftFor | -0.000202751458924423 |

Numeric imputation medians:

| term | median |
| --- | --- |
| periodNumber | 3 |
| secondsElapsedInPeriod | 250 |
| secondsElapsedInGame | 3361 |
| secondsElapsedInSequence | 43 |
| xCoordNorm | 67 |
| yCoordNorm | 0 |
| dXCoordNorm | 14 |
| dYCoordNorm | 0 |
| distance | 27.6586333718787 |
| angle | 29.9816393688493 |
| dDistance | -16.3157296296383 |
| dAngle | 7.1250163489018 |
| dSecondsElapsedInSequence | 16 |
| dXCoordNormPerSecond | 1.03448275862069 |
| dYCoordNormPerSecond | 0 |
| dDistancePerSecond | -1.13629798466721 |
| dAnglePerSecond | 0.33344651541941 |
| goalsFor | 2 |
| goalsAgainst | 2 |
| goalDifferential | 0 |
| shotsFor | 23 |
| shotsAgainst | 23 |
| shotDifferential | 0 |
| fenwickFor | 34 |
| fenwickAgainst | 34 |
| fenwickDifferential | 1 |
| corsiFor | 48 |
| corsiAgainst | 47 |
| corsiDifferential | 1 |
| shooterHeight | 73 |
| shooterWeight | 200 |
| shooterAge | 27 |
| shooterSecondsElapsedInShift | 26 |
| shooterSecondsElapsedSinceLastShift | 154 |
| goalieHeight | 75 |
| goalieWeight | 201 |
| goalieAge | 28 |
| minSecondsElapsedInShiftFor | 15 |
| maxSecondsElapsedInShiftFor | 38 |
| avgSecondsElapsedInShiftFor | 27.6666666666667 |
| minSecondsElapsedInShiftAgainst | 20 |
| maxSecondsElapsedInShiftAgainst | 42 |
| avgSecondsElapsedInShiftAgainst | 32 |
| minSecondsElapsedSinceLastShiftFor | 91 |
| maxSecondsElapsedSinceLastShiftFor | 236 |
| avgSecondsElapsedSinceLastShiftFor | 161.75 |
| minSecondsElapsedSinceLastShiftAgainst | 100 |
| maxSecondsElapsedSinceLastShiftAgainst | 249 |
| avgSecondsElapsedSinceLastShiftAgainst | 167.5 |
| skaterCountFor | 4 |
| skaterCountAgainst | 4 |
| manDifferential | 0 |
| isPlayoff_yes | 0 |
| isPlayoff_unknown | 0 |
| isPlayoff_new | 0 |
| isHome_yes | 1 |
| isHome_unknown | 0 |
| isHome_new | 0 |
| isOvertime_yes | 0 |
| isOvertime_unknown | 0 |
| isOvertime_new | 0 |
| zoneCode_N | 0 |
| zoneCode_O | 1 |
| zoneCode_unknown | 0 |
| zoneCode_new | 0 |
| isBehindNet_yes | 0 |
| isBehindNet_unknown | 0 |
| isBehindNet_new | 0 |
| crossedRoyalRoad_yes | 0 |
| crossedRoyalRoad_unknown | 0 |
| crossedRoyalRoad_new | 0 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0 |
| typeDescKeyPrev_blocked.shot.against | 0 |
| typeDescKeyPrev_blocked.shot.for | 0 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0 |
| typeDescKeyPrev_deflected.shot.on.goal.for | 0 |
| typeDescKeyPrev_giveaway.against | 0 |
| typeDescKeyPrev_giveaway.for | 0 |
| typeDescKeyPrev_given.hit | 0 |
| typeDescKeyPrev_high.missed.shot.against | 0 |
| typeDescKeyPrev_high.missed.shot.for | 0 |
| typeDescKeyPrev_lost.faceoff | 0 |
| typeDescKeyPrev_other.missed.shot.against | 0 |
| typeDescKeyPrev_other.missed.shot.for | 0 |
| typeDescKeyPrev_other.shot.on.goal.against | 0 |
| typeDescKeyPrev_other.shot.on.goal.for | 0 |
| typeDescKeyPrev_post.missed.shot.against | 0 |
| typeDescKeyPrev_post.missed.shot.for | 0 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0 |
| typeDescKeyPrev_takeaway.against | 0 |
| typeDescKeyPrev_takeaway.for | 0 |
| typeDescKeyPrev_taken.hit | 0 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0 |
| typeDescKeyPrev_wide.missed.shot.against | 0 |
| typeDescKeyPrev_wide.missed.shot.for | 0 |
| typeDescKeyPrev_won.faceoff | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0 |
| typeDescKeyPrev_unknown | 0 |
| typeDescKeyPrev_new | 0 |
| shotType_deflected | 0 |
| shotType_other | 0 |
| shotType_slap | 0 |
| shotType_snap | 0 |
| shotType_tip.in | 0 |
| shotType_wrist | 1 |
| shotType_unknown | 0 |
| shotType_new | 0 |
| isRebound_yes | 0 |
| isRebound_unknown | 0 |
| isRebound_new | 0 |
| isRush_yes | 0 |
| isRush_unknown | 0 |
| isRush_new | 0 |
| shooterHandCode_R | 0 |
| shooterHandCode_unknown | 0 |
| shooterHandCode_new | 0 |
| shooterPositionCode_D | 0 |
| shooterPositionCode_L | 0 |
| shooterPositionCode_R | 0 |
| shooterPositionCode_unknown | 0 |
| shooterPositionCode_new | 0 |
| goalieHandCode_R | 0 |
| goalieHandCode_unknown | 0 |
| goalieHandCode_new | 0 |
| isEmptyNetFor_yes | 0 |
| isEmptyNetFor_unknown | 0 |
| isEmptyNetFor_new | 0 |
| strengthState_penalty.kill | 0 |
| strengthState_unknown | 0 |
| strengthState_new | 0 |

Numeric normalization parameters:

| term | mean | sd |
| --- | --- | --- |
| periodNumber | 2.98349296922763 | 1.11119116594684 |
| secondsElapsedInPeriod | 400.825147748115 | 356.229641775825 |
| secondsElapsedInGame | 2781.01671082128 | 1121.91012087837 |
| secondsElapsedInSequence | 58.2645200733646 | 50.5226831333774 |
| xCoordNorm | 65.8879152231506 | 15.9694619992536 |
| yCoordNorm | 0.325045852863257 | 15.5189779789177 |
| dXCoordNorm | 37.4465049928673 | 58.1208952498798 |
| dYCoordNorm | -0.299368249439576 | 24.6208808898713 |
| distance | 27.8586142599961 | 15.9369015419197 |
| angle | 31.8344602691265 | 21.5289170355125 |
| dDistance | -37.5063164835068 | 55.9433462233817 |
| dAngle | 5.67286802861945 | 32.8160068465335 |
| dSecondsElapsedInSequence | 20.5822294681068 | 17.9630206867085 |
| dXCoordNormPerSecond | 3.97075861963049 | 13.572748286326 |
| dYCoordNormPerSecond | -0.129805649447885 | 5.54363699087283 |
| dDistancePerSecond | -4.10763502451766 | 13.3798084173814 |
| dAnglePerSecond | 0.508385002411622 | 8.76467463977103 |
| goalsFor | 2.0228245363766 | 1.44045020394245 |
| goalsAgainst | 2.06134094151213 | 1.47215882482309 |
| goalDifferential | -0.0385164051355207 | 1.37457849592714 |
| shotsFor | 22.3499082942735 | 10.5767843311353 |
| shotsAgainst | 21.8838394130834 | 10.2703149828133 |
| shotDifferential | 0.466068881190137 | 8.52189693485358 |
| fenwickFor | 32.6926839209293 | 15.1559804649857 |
| fenwickAgainst | 31.9643366619116 | 14.6523148337361 |
| fenwickDifferential | 0.72834725901773 | 12.0996245633173 |
| corsiFor | 45.7943753821072 | 20.9669508969354 |
| corsiAgainst | 44.6804564907275 | 20.1724271044118 |
| corsiDifferential | 1.11391889137966 | 16.4404403763273 |
| shooterHeight | 73.1120847768494 | 2.12656759152095 |
| shooterWeight | 199.8606072957 | 15.5221962464315 |
| shooterAge | 27.3908701854494 | 4.08442077490945 |
| shooterSecondsElapsedInShift | 29.995516608926 | 20.4893253772813 |
| shooterSecondsElapsedSinceLastShift | 207.688200529855 | 137.693628959127 |
| goalieHeight | 74.9127776645608 | 1.58639425318721 |
| goalieWeight | 201.366822906053 | 15.6508852036486 |
| goalieAge | 28.4369268392093 | 3.72826569808521 |
| minSecondsElapsedInShiftFor | 18.0063175056042 | 14.4088628634249 |
| maxSecondsElapsedInShiftFor | 41.7004279600571 | 24.0740874283492 |
| avgSecondsElapsedInShiftFor | 29.169234427009 | 14.7480386090541 |
| minSecondsElapsedInShiftAgainst | 23.3124108416548 | 16.4717084049645 |
| maxSecondsElapsedInShiftAgainst | 46.2822498471571 | 26.0750824227143 |
| avgSecondsElapsedInShiftAgainst | 34.1134671557639 | 17.4975938159751 |
| minSecondsElapsedSinceLastShiftFor | 152.142041980844 | 128.584651638218 |
| maxSecondsElapsedSinceLastShiftFor | 269.020786631343 | 134.032931469307 |
| avgSecondsElapsedSinceLastShiftFor | 207.188818694382 | 113.823251941886 |
| minSecondsElapsedSinceLastShiftAgainst | 162.128591807622 | 131.206123532942 |
| maxSecondsElapsedSinceLastShiftAgainst | 274.253719176686 | 134.651782579737 |
| avgSecondsElapsedSinceLastShiftAgainst | 214.484104340737 | 116.340521180235 |
| skaterCountFor | 3.55308742612594 | 0.510173852188287 |
| skaterCountAgainst | 3.55308742612594 | 0.510173852188287 |
| manDifferential | -0.00652129610760138 | 0.0804990023873362 |
| isPlayoff_yes | 0.0421846341960465 | 0.20103066105941 |
| isHome_yes | 0.508661096392908 | 0.499975927550008 |
| isOvertime_yes | 0.476258406358264 | 0.499486916594719 |
| zoneCode_N | 0.00570613409415121 | 0.0753308076689253 |
| zoneCode_O | 0.991644589362136 | 0.0910345353217617 |
| isBehindNet_yes | 0.014265335235378 | 0.118594695069551 |
| crossedRoyalRoad_yes | 0.414306093336051 | 0.492652022772426 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0.01202363969839 | 0.109002262005236 |
| typeDescKeyPrev_blocked.shot.against | 0.0425922152027716 | 0.20195650594633 |
| typeDescKeyPrev_blocked.shot.for | 0.0568575504381496 | 0.231593825139162 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0.000815162013450173 | 0.0285423114872626 |
| typeDescKeyPrev_deflected.shot.on.goal.for | 0.00163032402690034 | 0.0403484552488342 |
| typeDescKeyPrev_giveaway.against | 0.0538006928877114 | 0.225646969053026 |
| typeDescKeyPrev_giveaway.for | 0.0167108212757285 | 0.128198748076715 |
| typeDescKeyPrev_given.hit | 0.033014061544732 | 0.178691467189516 |
| typeDescKeyPrev_high.missed.shot.against | 0.00489097208070103 | 0.069771358978061 |
| typeDescKeyPrev_high.missed.shot.for | 0.00489097208070103 | 0.0697713589780609 |
| typeDescKeyPrev_lost.faceoff | 0.103321785204809 | 0.304409720952863 |
| typeDescKeyPrev_other.missed.shot.against | 0.00101895251681271 | 0.0319080199342157 |
| typeDescKeyPrev_other.missed.shot.for | 0.0014265335235378 | 0.0377463757742101 |
| typeDescKeyPrev_other.shot.on.goal.against | 0.00122274302017525 | 0.0349499191578386 |
| typeDescKeyPrev_other.shot.on.goal.for | 0.00183411453026288 | 0.0427916314222468 |
| typeDescKeyPrev_post.missed.shot.against | 0.00692887711432647 | 0.0829594498390242 |
| typeDescKeyPrev_post.missed.shot.for | 0.0101895251681272 | 0.10043781426111 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0.00652129610760138 | 0.0804990023873367 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0.00611371510087629 | 0.0779588105402005 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0.0197676788261667 | 0.139215183570981 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0.0234359078866925 | 0.15129881408694 |
| typeDescKeyPrev_takeaway.against | 0.0169146117790911 | 0.128964712654812 |
| typeDescKeyPrev_takeaway.for | 0.0478907682901977 | 0.213556870135017 |
| typeDescKeyPrev_taken.hit | 0.0360709190951702 | 0.186485643125038 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0.00326064805380069 | 0.0570147234099724 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0.00509476258406358 | 0.0712028030582424 |
| typeDescKeyPrev_wide.missed.shot.against | 0.0381088241287956 | 0.191478493387823 |
| typeDescKeyPrev_wide.missed.shot.for | 0.0605257794986754 | 0.238482703556919 |
| typeDescKeyPrev_won.faceoff | 0.243733442021602 | 0.429377483227489 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0.0548196454045242 | 0.227651078029084 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0.0766252292643163 | 0.2660229790766 |
| shotType_deflected | 0.01202363969839 | 0.109002262005236 |
| shotType_other | 0.016507030772366 | 0.12742785342564 |
| shotType_slap | 0.0672508661096393 | 0.250481482612257 |
| shotType_snap | 0.203586712859181 | 0.402706111653609 |
| shotType_tip.in | 0.049521092317098 | 0.216975454494338 |
| shotType_wrist | 0.567760342368046 | 0.495437743944978 |
| isRebound_yes | 0.0641940085592011 | 0.245123198932045 |
| isRush_yes | 0.0285306704707561 | 0.166500212789562 |
| shooterHandCode_R | 0.387609537395557 | 0.487254314704404 |
| shooterPositionCode_D | 0.318320766252293 | 0.46587217783541 |
| shooterPositionCode_L | 0.154269411045445 | 0.361243621260022 |
| shooterPositionCode_R | 0.172814346851437 | 0.378125225384581 |
| goalieHandCode_R | 0.0519665783574485 | 0.221982195438042 |
| isEmptyNetFor_yes | 0.00652129610760138 | 0.0804990023873362 |
| strengthState_penalty.kill | 0.00652129610760138 | 0.0804990023873362 |

Dummy map:

| variable | level | output_column |
| --- | --- | --- |
| isPlayoff | yes | isPlayoff_yes |
| isPlayoff | unknown | isPlayoff_unknown |
| isPlayoff | new | isPlayoff_new |
| isHome | yes | isHome_yes |
| isHome | unknown | isHome_unknown |
| isHome | new | isHome_new |
| isOvertime | yes | isOvertime_yes |
| isOvertime | unknown | isOvertime_unknown |
| isOvertime | new | isOvertime_new |
| zoneCode | N | zoneCode_N |
| zoneCode | O | zoneCode_O |
| zoneCode | unknown | zoneCode_unknown |
| zoneCode | new | zoneCode_new |
| isBehindNet | yes | isBehindNet_yes |
| isBehindNet | unknown | isBehindNet_unknown |
| isBehindNet | new | isBehindNet_new |
| crossedRoyalRoad | yes | crossedRoyalRoad_yes |
| crossedRoyalRoad | unknown | crossedRoyalRoad_unknown |
| crossedRoyalRoad | new | crossedRoyalRoad_new |
| typeDescKeyPrev | backhand-shot-on-goal-for | typeDescKeyPrev_backhand.shot.on.goal.for |
| typeDescKeyPrev | blocked-shot-against | typeDescKeyPrev_blocked.shot.against |
| typeDescKeyPrev | blocked-shot-for | typeDescKeyPrev_blocked.shot.for |
| typeDescKeyPrev | deflected-shot-on-goal-against | typeDescKeyPrev_deflected.shot.on.goal.against |
| typeDescKeyPrev | deflected-shot-on-goal-for | typeDescKeyPrev_deflected.shot.on.goal.for |
| typeDescKeyPrev | giveaway-against | typeDescKeyPrev_giveaway.against |
| typeDescKeyPrev | giveaway-for | typeDescKeyPrev_giveaway.for |
| typeDescKeyPrev | given-hit | typeDescKeyPrev_given.hit |
| typeDescKeyPrev | high-missed-shot-against | typeDescKeyPrev_high.missed.shot.against |
| typeDescKeyPrev | high-missed-shot-for | typeDescKeyPrev_high.missed.shot.for |
| typeDescKeyPrev | lost-faceoff | typeDescKeyPrev_lost.faceoff |
| typeDescKeyPrev | other-missed-shot-against | typeDescKeyPrev_other.missed.shot.against |
| typeDescKeyPrev | other-missed-shot-for | typeDescKeyPrev_other.missed.shot.for |
| typeDescKeyPrev | other-shot-on-goal-against | typeDescKeyPrev_other.shot.on.goal.against |
| typeDescKeyPrev | other-shot-on-goal-for | typeDescKeyPrev_other.shot.on.goal.for |
| typeDescKeyPrev | post-missed-shot-against | typeDescKeyPrev_post.missed.shot.against |
| typeDescKeyPrev | post-missed-shot-for | typeDescKeyPrev_post.missed.shot.for |
| typeDescKeyPrev | slap-shot-on-goal-against | typeDescKeyPrev_slap.shot.on.goal.against |
| typeDescKeyPrev | slap-shot-on-goal-for | typeDescKeyPrev_slap.shot.on.goal.for |
| typeDescKeyPrev | snap-shot-on-goal-against | typeDescKeyPrev_snap.shot.on.goal.against |
| typeDescKeyPrev | snap-shot-on-goal-for | typeDescKeyPrev_snap.shot.on.goal.for |
| typeDescKeyPrev | takeaway-against | typeDescKeyPrev_takeaway.against |
| typeDescKeyPrev | takeaway-for | typeDescKeyPrev_takeaway.for |
| typeDescKeyPrev | taken-hit | typeDescKeyPrev_taken.hit |
| typeDescKeyPrev | tip-in-shot-on-goal-against | typeDescKeyPrev_tip.in.shot.on.goal.against |
| typeDescKeyPrev | tip-in-shot-on-goal-for | typeDescKeyPrev_tip.in.shot.on.goal.for |
| typeDescKeyPrev | wide-missed-shot-against | typeDescKeyPrev_wide.missed.shot.against |
| typeDescKeyPrev | wide-missed-shot-for | typeDescKeyPrev_wide.missed.shot.for |
| typeDescKeyPrev | won-faceoff | typeDescKeyPrev_won.faceoff |
| typeDescKeyPrev | wrist-shot-on-goal-against | typeDescKeyPrev_wrist.shot.on.goal.against |
| typeDescKeyPrev | wrist-shot-on-goal-for | typeDescKeyPrev_wrist.shot.on.goal.for |
| typeDescKeyPrev | unknown | typeDescKeyPrev_unknown |
| typeDescKeyPrev | new | typeDescKeyPrev_new |
| shotType | deflected | shotType_deflected |
| shotType | other | shotType_other |
| shotType | slap | shotType_slap |
| shotType | snap | shotType_snap |
| shotType | tip-in | shotType_tip.in |
| shotType | wrist | shotType_wrist |
| shotType | unknown | shotType_unknown |
| shotType | new | shotType_new |
| isRebound | yes | isRebound_yes |
| isRebound | unknown | isRebound_unknown |
| isRebound | new | isRebound_new |
| isRush | yes | isRush_yes |
| isRush | unknown | isRush_unknown |
| isRush | new | isRush_new |
| shooterHandCode | R | shooterHandCode_R |
| shooterHandCode | unknown | shooterHandCode_unknown |
| shooterHandCode | new | shooterHandCode_new |
| shooterPositionCode | D | shooterPositionCode_D |
| shooterPositionCode | L | shooterPositionCode_L |
| shooterPositionCode | R | shooterPositionCode_R |
| shooterPositionCode | unknown | shooterPositionCode_unknown |
| shooterPositionCode | new | shooterPositionCode_new |
| goalieHandCode | R | goalieHandCode_R |
| goalieHandCode | unknown | goalieHandCode_unknown |
| goalieHandCode | new | goalieHandCode_new |
| isEmptyNetFor | yes | isEmptyNetFor_yes |
| isEmptyNetFor | unknown | isEmptyNetFor_unknown |
| isEmptyNetFor | new | isEmptyNetFor_new |
| strengthState | penalty-kill | strengthState_penalty.kill |
| strengthState | unknown | strengthState_unknown |
| strengthState | new | strengthState_new |

Unknown categorical fills:

| variable | value |
| --- | --- |
| isPlayoff | unknown |
| isHome | unknown |
| isOvertime | unknown |
| zoneCode | unknown |
| isBehindNet | unknown |
| crossedRoyalRoad | unknown |
| typeDescKeyPrev | unknown |
| shotType | unknown |
| isRebound | unknown |
| isRush | unknown |
| shooterHandCode | unknown |
| shooterPositionCode | unknown |
| goalieHandCode | unknown |
| isEmptyNetFor | unknown |
| strengthState | unknown |

Novel categorical buckets:

| variable | value |
| --- | --- |
| isPlayoff | new |
| isHome | new |
| isOvertime | new |
| zoneCode | new |
| isBehindNet | new |
| crossedRoyalRoad | new |
| typeDescKeyPrev | new |
| shotType | new |
| isRebound | new |
| isRush | new |
| shooterHandCode | new |
| shooterPositionCode | new |
| goalieHandCode | new |
| isEmptyNetFor | new |
| strengthState | new |

Zero-variance terms removed before fitting:

| term |
| --- |
| isPlayoff_unknown |
| isPlayoff_new |
| isHome_unknown |
| isHome_new |
| isOvertime_unknown |
| isOvertime_new |
| zoneCode_unknown |
| zoneCode_new |
| isBehindNet_unknown |
| isBehindNet_new |
| crossedRoyalRoad_unknown |
| crossedRoyalRoad_new |
| typeDescKeyPrev_unknown |
| typeDescKeyPrev_new |
| shotType_unknown |
| shotType_new |
| isRebound_unknown |
| isRebound_new |
| isRush_unknown |
| isRush_new |
| shooterHandCode_unknown |
| shooterHandCode_new |
| shooterPositionCode_unknown |
| shooterPositionCode_new |
| goalieHandCode_unknown |
| goalieHandCode_new |
| isEmptyNetFor_unknown |
| isEmptyNetFor_new |
| strengthState_unknown |
| strengthState_new |

## PP: Power Play

- Raw predictor columns: `isPlayoff`, `isHome`, `isOvertime`, `periodNumber`, `secondsElapsedInPeriod`, `secondsElapsedInGame`, `secondsElapsedInSequence`, `zoneCode`, `xCoordNorm`, `yCoordNorm`, `dXCoordNorm`, `dYCoordNorm`, `distance`, `angle`, `dDistance`, `dAngle`, `dSecondsElapsedInSequence`, `dXCoordNormPerSecond`, `dYCoordNormPerSecond`, `dDistancePerSecond`, `dAnglePerSecond`, `isBehindNet`, `crossedRoyalRoad`, `typeDescKeyPrev`, `shotType`, `isRebound`, `isRush`, `goalsFor`, `goalsAgainst`, `goalDifferential`, `shotsFor`, `shotsAgainst`, `shotDifferential`, `fenwickFor`, `fenwickAgainst`, `fenwickDifferential`, `corsiFor`, `corsiAgainst`, `corsiDifferential`, `shooterHeight`, `shooterWeight`, `shooterHandCode`, `shooterPositionCode`, `shooterAge`, `shooterSecondsElapsedInShift`, `shooterSecondsElapsedSinceLastShift`, `goalieHeight`, `goalieWeight`, `goalieHandCode`, `goalieAge`, `minSecondsElapsedInShiftFor`, `maxSecondsElapsedInShiftFor`, `avgSecondsElapsedInShiftFor`, `minSecondsElapsedInShiftAgainst`, `maxSecondsElapsedInShiftAgainst`, `avgSecondsElapsedInShiftAgainst`, `minSecondsElapsedSinceLastShiftFor`, `maxSecondsElapsedSinceLastShiftFor`, `avgSecondsElapsedSinceLastShiftFor`, `minSecondsElapsedSinceLastShiftAgainst`, `maxSecondsElapsedSinceLastShiftAgainst`, `avgSecondsElapsedSinceLastShiftAgainst`, `isEmptyNetFor`, `skaterCountFor`, `skaterCountAgainst`, `manDifferential`, `strengthState`
- Best penalty (`glmnet` lambda): `0.0000001`
- Final searched log10 range: `[-7, 2]`
- Search note: `accepted_lower_boundary`

Training summary:

| seasons | games | rows | goal_rate |
| --- | --- | --- | --- |
| 2023,2024 | 2793 | 38903 | 0.0972932678713724 |

Best-penalty grouped CV metrics:

| .metric | mean | std_err |
| --- | --- | --- |
| mn_log_loss | 0.303556139340419 | 0.00285175944653909 |
| roc_auc | 0.669264538566136 | 0.00314426950822314 |
| pr_auc | 0.946342577289955 | 0.00110247913842704 |
| brier_class | 0.0851807617637989 | 0.000969808631869044 |

Top grouped CV log-loss settings:

| penalty | .metric | .estimator | mean | n | std_err | .config |
| --- | --- | --- | --- | --- | --- | --- |
| 0.0000001 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod01_post0 |
| 0.000000204335971785694 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod02_post0 |
| 0.00000041753189365604 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod03_post0 |
| 0.000000853167852417281 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod04_post0 |
| 0.00000174332882219999 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod05_post0 |
| 0.00000356224789026244 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod06_post0 |
| 0.00000727895384398316 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod07_post0 |
| 0.0000148735210729351 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod08_post0 |
| 0.000030391953823132 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod09_post0 |
| 0.0000621016941891562 | mn_log_loss | binary | 0.303556139340419 | 5 | 0.00285175944653909 | pre0_mod10_post0 |

Penalty search history:

| iteration | log10_lower | log10_upper | penalty_lower | penalty_upper | best_penalty | best_cv_mn_log_loss | boundary_hit |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | -7 | 2 | 0.0000001 | 100 | 0.0000001 | 0.303556139340419 | accepted_lower_boundary |

Coefficients:

| term | estimate |
| --- | --- |
| (Intercept) | -2.39794537616407 |
| distance | -0.542106800055756 |
| angle | -0.219305609664214 |
| shotType_slap | 0.202036487974767 |
| shotType_snap | 0.199877896687757 |
| xCoordNorm | 0.195416091557338 |
| dDistancePerSecond | 0.114983160197665 |
| skaterCountAgainst | -0.0823652979432172 |
| isBehindNet_yes | -0.0788761579025495 |
| shotType_wrist | 0.0755015126064845 |
| fenwickDifferential | -0.0746174856353005 |
| dAnglePerSecond | 0.0725611059967527 |
| strengthState_power.play | -0.067393405706683 |
| shotType_tip.in | -0.0633953127911626 |
| manDifferential | 0.0624546013150301 |
| shooterSecondsElapsedInShift | 0.0615406525869288 |
| dDistance | -0.0589694478055236 |
| zoneCode_O | 0.0542404665696632 |
| dXCoordNormPerSecond | -0.0504091965240544 |
| crossedRoyalRoad_yes | 0.0481463889935093 |
| shotDifferential | 0.0476986903604757 |
| corsiDifferential | 0.0462758034404314 |
| fenwickAgainst | 0.0458411382315917 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | -0.0446208659235661 |
| avgSecondsElapsedInShiftAgainst | 0.0413296131380018 |
| zoneCode_N | -0.0410588612731695 |
| dYCoordNormPerSecond | 0.0402477252152678 |
| shooterPositionCode_D | -0.0379709273985291 |
| typeDescKeyPrev_other.missed.shot.against | -0.037246808898672 |
| maxSecondsElapsedInShiftFor | -0.037097395996207 |
| typeDescKeyPrev_given.hit | -0.0355153896250407 |
| secondsElapsedInPeriod | -0.0344073679576721 |
| corsiAgainst | -0.0343954059796835 |
| goalsFor | 0.0339613303218139 |
| shotsAgainst | -0.0335533600182796 |
| typeDescKeyPrev_backhand.shot.on.goal.for | -0.032805613923629 |
| goalieWeight | -0.0297207929759291 |
| avgSecondsElapsedSinceLastShiftAgainst | 0.0269696678931352 |
| shooterHandCode_R | 0.0268083164640094 |
| dXCoordNorm | -0.0260663087297142 |
| shotType_deflected | -0.0259998910725034 |
| minSecondsElapsedInShiftAgainst | 0.0256311907642271 |
| typeDescKeyPrev_post.missed.shot.for | 0.0253879709227095 |
| shooterSecondsElapsedSinceLastShift | -0.0250529788447465 |
| typeDescKeyPrev_wide.missed.shot.for | 0.0247145244859805 |
| secondsElapsedInSequence | 0.0237154247251768 |
| maxSecondsElapsedSinceLastShiftAgainst | -0.0236837967916478 |
| avgSecondsElapsedSinceLastShiftFor | 0.0235508188587901 |
| goalieHandCode_R | -0.023197073829993 |
| typeDescKeyPrev_takeaway.against | 0.0228972272047815 |
| typeDescKeyPrev_blocked.shot.for | -0.0228040798652185 |
| typeDescKeyPrev_giveaway.for | -0.0212038363040511 |
| goalDifferential | 0.0205691205270079 |
| typeDescKeyPrev_blocked.shot.against | -0.0202413355374004 |
| avgSecondsElapsedInShiftFor | 0.0190890497620023 |
| typeDescKeyPrev_takeaway.for | 0.0187136266828988 |
| typeDescKeyPrev_high.missed.shot.against | -0.0186420091695272 |
| typeDescKeyPrev_slap.shot.on.goal.against | -0.0175474152515582 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0.0174773767181774 |
| skaterCountFor | -0.0162033863552251 |
| goalieAge | 0.0161882932870087 |
| typeDescKeyPrev_snap.shot.on.goal.against | -0.0158210471316226 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0.015553550408886 |
| shooterPositionCode_L | -0.0144470505604948 |
| dAngle | -0.0134820029532234 |
| periodNumber | 0.0134641612028405 |
| shotType_other | 0.0130618100280583 |
| shooterWeight | 0.0128247783443267 |
| yCoordNorm | 0.0124301714195014 |
| isPlayoff_yes | 0.0122451408116191 |
| typeDescKeyPrev_high.missed.shot.for | 0.0121376776309437 |
| typeDescKeyPrev_other.shot.on.goal.for | -0.0118124537450564 |
| typeDescKeyPrev_wide.missed.shot.against | -0.0112739987445016 |
| minSecondsElapsedInShiftFor | -0.0111735183298314 |
| isRebound_yes | -0.0110276054335823 |
| typeDescKeyPrev_post.missed.shot.against | -0.010720267914786 |
| isRush_yes | 0.0105864499329921 |
| typeDescKeyPrev_giveaway.against | 0.0103474537529314 |
| isEmptyNetFor_yes | 0.0100854358139979 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0.00986326434684355 |
| maxSecondsElapsedInShiftAgainst | -0.00965757107677408 |
| typeDescKeyPrev_won.faceoff | 0.00929690665838444 |
| typeDescKeyPrev_other.missed.shot.for | 0.00884083982103484 |
| fenwickFor | -0.00863614770668811 |
| typeDescKeyPrev_other.shot.on.goal.against | 0.00824295732759891 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0.00783379943286155 |
| shooterPositionCode_R | -0.00746768841558718 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0.00723726245124362 |
| isOvertime_yes | 0.00721993922043485 |
| dSecondsElapsedInSequence | -0.00713380900491467 |
| goalsAgainst | 0.00710288122029498 |
| isHome_yes | -0.00652956519033584 |
| shooterAge | 0.00617256884314132 |
| maxSecondsElapsedSinceLastShiftFor | -0.00496285313036934 |
| minSecondsElapsedSinceLastShiftFor | -0.00345997628850636 |
| typeDescKeyPrev_taken.hit | 0.00319159220161459 |
| typeDescKeyPrev_deflected.shot.on.goal.for | -0.00315296353977997 |
| goalieHeight | -0.00297766734197942 |
| dYCoordNorm | -0.00286091648778375 |
| shotsFor | 0.0028290286205774 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0.0027889155387361 |
| minSecondsElapsedSinceLastShiftAgainst | -0.0019951017248615 |
| typeDescKeyPrev_lost.faceoff | -0.000921338045609677 |
| corsiFor | -0.000833251525310769 |
| secondsElapsedInGame | 0.000747769760615429 |
| shooterHeight | -0.000418054512339294 |

Numeric imputation medians:

| term | median |
| --- | --- |
| periodNumber | 2 |
| secondsElapsedInPeriod | 695 |
| secondsElapsedInGame | 2025 |
| secondsElapsedInSequence | 38 |
| xCoordNorm | 67 |
| yCoordNorm | 0 |
| dXCoordNorm | 8 |
| dYCoordNorm | -1 |
| distance | 29.0688837074973 |
| angle | 30.1735200296443 |
| dDistance | -9.10318436834253 |
| dAngle | 2.4681179138285 |
| dSecondsElapsedInSequence | 13 |
| dXCoordNormPerSecond | 0.625 |
| dYCoordNormPerSecond | -0.0416666666666667 |
| dDistancePerSecond | -0.716258411284149 |
| dAnglePerSecond | 0.12986072269337 |
| goalsFor | 1 |
| goalsAgainst | 1 |
| goalDifferential | 0 |
| shotsFor | 16 |
| shotsAgainst | 15 |
| shotDifferential | 1 |
| fenwickFor | 24 |
| fenwickAgainst | 23 |
| fenwickDifferential | 1 |
| corsiFor | 33 |
| corsiAgainst | 32 |
| corsiDifferential | 2 |
| shooterHeight | 73 |
| shooterWeight | 200 |
| shooterAge | 28 |
| shooterSecondsElapsedInShift | 43 |
| shooterSecondsElapsedSinceLastShift | 142 |
| goalieHeight | 75 |
| goalieWeight | 201 |
| goalieAge | 28 |
| minSecondsElapsedInShiftFor | 30 |
| maxSecondsElapsedInShiftFor | 61 |
| avgSecondsElapsedInShiftFor | 44.3333333333333 |
| minSecondsElapsedInShiftAgainst | 24 |
| maxSecondsElapsedInShiftAgainst | 46 |
| avgSecondsElapsedInShiftAgainst | 34.5 |
| minSecondsElapsedSinceLastShiftFor | 90 |
| maxSecondsElapsedSinceLastShiftFor | 219 |
| avgSecondsElapsedSinceLastShiftFor | 151.2 |
| minSecondsElapsedSinceLastShiftAgainst | 75 |
| maxSecondsElapsedSinceLastShiftAgainst | 191 |
| avgSecondsElapsedSinceLastShiftAgainst | 130.75 |
| skaterCountFor | 5 |
| skaterCountAgainst | 4 |
| manDifferential | 1 |
| isPlayoff_yes | 0 |
| isPlayoff_unknown | 0 |
| isPlayoff_new | 0 |
| isHome_yes | 1 |
| isHome_unknown | 0 |
| isHome_new | 0 |
| isOvertime_yes | 0 |
| isOvertime_unknown | 0 |
| isOvertime_new | 0 |
| zoneCode_N | 0 |
| zoneCode_O | 1 |
| zoneCode_unknown | 0 |
| zoneCode_new | 0 |
| isBehindNet_yes | 0 |
| isBehindNet_unknown | 0 |
| isBehindNet_new | 0 |
| crossedRoyalRoad_yes | 0 |
| crossedRoyalRoad_unknown | 0 |
| crossedRoyalRoad_new | 0 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0 |
| typeDescKeyPrev_blocked.shot.against | 0 |
| typeDescKeyPrev_blocked.shot.for | 0 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0 |
| typeDescKeyPrev_deflected.shot.on.goal.for | 0 |
| typeDescKeyPrev_giveaway.against | 0 |
| typeDescKeyPrev_giveaway.for | 0 |
| typeDescKeyPrev_given.hit | 0 |
| typeDescKeyPrev_high.missed.shot.against | 0 |
| typeDescKeyPrev_high.missed.shot.for | 0 |
| typeDescKeyPrev_lost.faceoff | 0 |
| typeDescKeyPrev_other.missed.shot.against | 0 |
| typeDescKeyPrev_other.missed.shot.for | 0 |
| typeDescKeyPrev_other.shot.on.goal.against | 0 |
| typeDescKeyPrev_other.shot.on.goal.for | 0 |
| typeDescKeyPrev_post.missed.shot.against | 0 |
| typeDescKeyPrev_post.missed.shot.for | 0 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0 |
| typeDescKeyPrev_takeaway.against | 0 |
| typeDescKeyPrev_takeaway.for | 0 |
| typeDescKeyPrev_taken.hit | 0 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0 |
| typeDescKeyPrev_wide.missed.shot.against | 0 |
| typeDescKeyPrev_wide.missed.shot.for | 0 |
| typeDescKeyPrev_won.faceoff | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0 |
| typeDescKeyPrev_unknown | 0 |
| typeDescKeyPrev_new | 0 |
| shotType_deflected | 0 |
| shotType_other | 0 |
| shotType_slap | 0 |
| shotType_snap | 0 |
| shotType_tip.in | 0 |
| shotType_wrist | 0 |
| shotType_unknown | 0 |
| shotType_new | 0 |
| isRebound_yes | 0 |
| isRebound_unknown | 0 |
| isRebound_new | 0 |
| isRush_yes | 0 |
| isRush_unknown | 0 |
| isRush_new | 0 |
| shooterHandCode_R | 0 |
| shooterHandCode_unknown | 0 |
| shooterHandCode_new | 0 |
| shooterPositionCode_D | 0 |
| shooterPositionCode_L | 0 |
| shooterPositionCode_R | 0 |
| shooterPositionCode_unknown | 0 |
| shooterPositionCode_new | 0 |
| goalieHandCode_R | 0 |
| goalieHandCode_unknown | 0 |
| goalieHandCode_new | 0 |
| isEmptyNetFor_yes | 0 |
| isEmptyNetFor_unknown | 0 |
| isEmptyNetFor_new | 0 |
| strengthState_power.play | 1 |
| strengthState_unknown | 0 |
| strengthState_new | 0 |

Numeric normalization parameters:

| term | mean | sd |
| --- | --- | --- |
| periodNumber | 2.12343521065213 | 0.819114862110249 |
| secondsElapsedInPeriod | 676.62771508624 | 354.515819287478 |
| secondsElapsedInGame | 2024.7499678688 | 1054.14905526037 |
| secondsElapsedInSequence | 46.8816029612112 | 38.2574560158682 |
| xCoordNorm | 64.7508418373904 | 17.1655125944257 |
| yCoordNorm | -0.143382258437653 | 16.1216873173314 |
| dXCoordNorm | 30.0947741819397 | 59.3431367166308 |
| dYCoordNorm | -0.891190910726679 | 25.4505533749231 |
| distance | 29.1282102447433 | 17.1507005274403 |
| angle | 31.9071696758191 | 22.043218799273 |
| dDistance | -30.6013269900116 | 57.1844184956307 |
| dAngle | 2.48690975205173 | 32.2265333216531 |
| dSecondsElapsedInSequence | 17.7524869547336 | 16.5579541267536 |
| dXCoordNormPerSecond | 3.7361414435976 | 15.3336003533296 |
| dYCoordNormPerSecond | -0.122577302991125 | 6.30974632573911 |
| dDistancePerSecond | -3.91373151316634 | 14.957584325209 |
| dAnglePerSecond | 0.397426256756507 | 11.523817846786 |
| goalsFor | 1.36760661131532 | 1.34474356249459 |
| goalsAgainst | 1.69151479320361 | 1.56127620550749 |
| goalDifferential | -0.323908181888286 | 1.68536534670059 |
| shotsFor | 16.6896640361926 | 9.92213828674564 |
| shotsAgainst | 15.8904968768475 | 9.67070746200474 |
| shotDifferential | 0.799167159345038 | 7.3311155060575 |
| fenwickFor | 24.5750713312598 | 14.2909359183026 |
| fenwickAgainst | 23.1451559005732 | 13.7083798375497 |
| fenwickDifferential | 1.42991543068658 | 10.0188510104672 |
| corsiFor | 34.50713312598 | 19.9283223899407 |
| corsiAgainst | 32.1676991491659 | 18.7405060917839 |
| corsiDifferential | 2.33943397681413 | 13.6091326864314 |
| shooterHeight | 72.9683571961031 | 2.05165698358487 |
| shooterWeight | 199.49286687402 | 15.2224400764648 |
| shooterAge | 27.7768809603372 | 4.33599665205436 |
| shooterSecondsElapsedInShift | 49.4272678199625 | 34.4161560863624 |
| shooterSecondsElapsedSinceLastShift | 167.720252936791 | 102.951454168434 |
| goalieHeight | 74.9186695113487 | 1.59123245546106 |
| goalieWeight | 201.744492712644 | 15.7957148596283 |
| goalieAge | 28.5635298048994 | 3.7449561167066 |
| minSecondsElapsedInShiftFor | 34.7959540395342 | 25.7608045293953 |
| maxSecondsElapsedInShiftFor | 67.5986427781919 | 39.4569217802776 |
| avgSecondsElapsedInShiftFor | 49.0961044135414 | 28.935389746769 |
| minSecondsElapsedInShiftAgainst | 27.7292753772203 | 18.8763627806978 |
| maxSecondsElapsedInShiftAgainst | 51.3274811711179 | 31.6180704267315 |
| avgSecondsElapsedInShiftAgainst | 37.6785209366886 | 21.4157579763441 |
| minSecondsElapsedSinceLastShiftFor | 104.561730457805 | 74.5260461088412 |
| maxSecondsElapsedSinceLastShiftFor | 242.658098347171 | 110.617368597874 |
| avgSecondsElapsedSinceLastShiftFor | 166.518160124755 | 74.8393266862022 |
| minSecondsElapsedSinceLastShiftAgainst | 93.0576048119682 | 72.0686565911692 |
| maxSecondsElapsedSinceLastShiftAgainst | 217.952677171426 | 114.878657304273 |
| avgSecondsElapsedSinceLastShiftAgainst | 148.38505256664 | 76.1908738637922 |
| skaterCountFor | 5.12968151556435 | 0.377533317658959 |
| skaterCountAgainst | 4.08749967868802 | 0.398836126582249 |
| manDifferential | 0.895560753669383 | 0.377734349046219 |
| isPlayoff_yes | 0.0642881011747166 | 0.245268602856523 |
| isHome_yes | 0.524715317584762 | 0.49939519799383 |
| isOvertime_yes | 0.0093566048890831 | 0.0962771888919389 |
| zoneCode_N | 0.00457548261059558 | 0.0674882556222866 |
| zoneCode_O | 0.994370614091458 | 0.0748187130006805 |
| isBehindNet_yes | 0.0125440197414081 | 0.111296836060494 |
| crossedRoyalRoad_yes | 0.432408811659769 | 0.49541673389754 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0.011335886692543 | 0.105866295194452 |
| typeDescKeyPrev_blocked.shot.against | 0.00722309333470426 | 0.0846823747324932 |
| typeDescKeyPrev_blocked.shot.for | 0.138035627072462 | 0.344941808471905 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0.000179934709405444 | 0.0134129399240703 |
| typeDescKeyPrev_deflected.shot.on.goal.for | 0.00424131815027118 | 0.0649879829962111 |
| typeDescKeyPrev_giveaway.against | 0.0310515898516824 | 0.173459395924063 |
| typeDescKeyPrev_giveaway.for | 0.0265018121995733 | 0.160624186662383 |
| typeDescKeyPrev_given.hit | 0.0182505205254094 | 0.133857758851758 |
| typeDescKeyPrev_high.missed.shot.against | 0.000539804128216333 | 0.0232277120747747 |
| typeDescKeyPrev_high.missed.shot.for | 0.00822558671567745 | 0.0903224011162981 |
| typeDescKeyPrev_lost.faceoff | 0.123538030486081 | 0.329058002214253 |
| typeDescKeyPrev_other.missed.shot.against | 0.000231344626378428 | 0.0152084532916172 |
| typeDescKeyPrev_other.missed.shot.for | 0.0044983677351361 | 0.0669197096229699 |
| typeDescKeyPrev_other.shot.on.goal.against | 0.000308459501837905 | 0.0175605319187755 |
| typeDescKeyPrev_other.shot.on.goal.for | 0.00282754543351412 | 0.0531001214558336 |
| typeDescKeyPrev_post.missed.shot.against | 0.00107960825643266 | 0.0328400734542823 |
| typeDescKeyPrev_post.missed.shot.for | 0.013058118911138 | 0.113525044485787 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0.00177364213556795 | 0.042077806987142 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0.0310515898516824 | 0.173459395924074 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0.00352157931264941 | 0.0592390749200158 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0.0345474642058453 | 0.18263294967333 |
| typeDescKeyPrev_takeaway.against | 0.0155514998843277 | 0.123733763701499 |
| typeDescKeyPrev_takeaway.for | 0.0167339279747063 | 0.128274419067155 |
| typeDescKeyPrev_taken.hit | 0.045240726936226 | 0.207834342424738 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0.000436984294270365 | 0.0208998700244794 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0.0202555072873557 | 0.140874880108493 |
| typeDescKeyPrev_wide.missed.shot.against | 0.00753155283654216 | 0.0864582020025193 |
| typeDescKeyPrev_wide.missed.shot.for | 0.0988612703390484 | 0.298479496148364 |
| typeDescKeyPrev_won.faceoff | 0.218389327301236 | 0.413158343560833 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0.0185589800272472 | 0.134963004203841 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0.0940030331851014 | 0.291836687539129 |
| shotType_deflected | 0.0221576742153561 | 0.147198059243233 |
| shotType_other | 0.0129810040356785 | 0.113193758322168 |
| shotType_slap | 0.188160296121122 | 0.390845142956 |
| shotType_snap | 0.18201681104285 | 0.385863342085321 |
| shotType_tip.in | 0.113153227257538 | 0.316784080986995 |
| shotType_wrist | 0.434208158753824 | 0.495658903644123 |
| isRebound_yes | 0.116700511528674 | 0.321067207779879 |
| isRush_yes | 0.0253707940261676 | 0.157250604013113 |
| shooterHandCode_R | 0.405418605248953 | 0.490979181043799 |
| shooterPositionCode_D | 0.182042516001337 | 0.385884524196194 |
| shooterPositionCode_L | 0.220831298357453 | 0.4148123178696 |
| shooterPositionCode_R | 0.210395085211937 | 0.407594484480938 |
| goalieHandCode_R | 0.0564480888363365 | 0.230787935620726 |
| isEmptyNetFor_yes | 0.146621083206951 | 0.353732324702205 |
| strengthState_power.play | 0.870986813356296 | 0.335218843182982 |

Dummy map:

| variable | level | output_column |
| --- | --- | --- |
| isPlayoff | yes | isPlayoff_yes |
| isPlayoff | unknown | isPlayoff_unknown |
| isPlayoff | new | isPlayoff_new |
| isHome | yes | isHome_yes |
| isHome | unknown | isHome_unknown |
| isHome | new | isHome_new |
| isOvertime | yes | isOvertime_yes |
| isOvertime | unknown | isOvertime_unknown |
| isOvertime | new | isOvertime_new |
| zoneCode | N | zoneCode_N |
| zoneCode | O | zoneCode_O |
| zoneCode | unknown | zoneCode_unknown |
| zoneCode | new | zoneCode_new |
| isBehindNet | yes | isBehindNet_yes |
| isBehindNet | unknown | isBehindNet_unknown |
| isBehindNet | new | isBehindNet_new |
| crossedRoyalRoad | yes | crossedRoyalRoad_yes |
| crossedRoyalRoad | unknown | crossedRoyalRoad_unknown |
| crossedRoyalRoad | new | crossedRoyalRoad_new |
| typeDescKeyPrev | backhand-shot-on-goal-for | typeDescKeyPrev_backhand.shot.on.goal.for |
| typeDescKeyPrev | blocked-shot-against | typeDescKeyPrev_blocked.shot.against |
| typeDescKeyPrev | blocked-shot-for | typeDescKeyPrev_blocked.shot.for |
| typeDescKeyPrev | deflected-shot-on-goal-against | typeDescKeyPrev_deflected.shot.on.goal.against |
| typeDescKeyPrev | deflected-shot-on-goal-for | typeDescKeyPrev_deflected.shot.on.goal.for |
| typeDescKeyPrev | giveaway-against | typeDescKeyPrev_giveaway.against |
| typeDescKeyPrev | giveaway-for | typeDescKeyPrev_giveaway.for |
| typeDescKeyPrev | given-hit | typeDescKeyPrev_given.hit |
| typeDescKeyPrev | high-missed-shot-against | typeDescKeyPrev_high.missed.shot.against |
| typeDescKeyPrev | high-missed-shot-for | typeDescKeyPrev_high.missed.shot.for |
| typeDescKeyPrev | lost-faceoff | typeDescKeyPrev_lost.faceoff |
| typeDescKeyPrev | other-missed-shot-against | typeDescKeyPrev_other.missed.shot.against |
| typeDescKeyPrev | other-missed-shot-for | typeDescKeyPrev_other.missed.shot.for |
| typeDescKeyPrev | other-shot-on-goal-against | typeDescKeyPrev_other.shot.on.goal.against |
| typeDescKeyPrev | other-shot-on-goal-for | typeDescKeyPrev_other.shot.on.goal.for |
| typeDescKeyPrev | post-missed-shot-against | typeDescKeyPrev_post.missed.shot.against |
| typeDescKeyPrev | post-missed-shot-for | typeDescKeyPrev_post.missed.shot.for |
| typeDescKeyPrev | slap-shot-on-goal-against | typeDescKeyPrev_slap.shot.on.goal.against |
| typeDescKeyPrev | slap-shot-on-goal-for | typeDescKeyPrev_slap.shot.on.goal.for |
| typeDescKeyPrev | snap-shot-on-goal-against | typeDescKeyPrev_snap.shot.on.goal.against |
| typeDescKeyPrev | snap-shot-on-goal-for | typeDescKeyPrev_snap.shot.on.goal.for |
| typeDescKeyPrev | takeaway-against | typeDescKeyPrev_takeaway.against |
| typeDescKeyPrev | takeaway-for | typeDescKeyPrev_takeaway.for |
| typeDescKeyPrev | taken-hit | typeDescKeyPrev_taken.hit |
| typeDescKeyPrev | tip-in-shot-on-goal-against | typeDescKeyPrev_tip.in.shot.on.goal.against |
| typeDescKeyPrev | tip-in-shot-on-goal-for | typeDescKeyPrev_tip.in.shot.on.goal.for |
| typeDescKeyPrev | wide-missed-shot-against | typeDescKeyPrev_wide.missed.shot.against |
| typeDescKeyPrev | wide-missed-shot-for | typeDescKeyPrev_wide.missed.shot.for |
| typeDescKeyPrev | won-faceoff | typeDescKeyPrev_won.faceoff |
| typeDescKeyPrev | wrist-shot-on-goal-against | typeDescKeyPrev_wrist.shot.on.goal.against |
| typeDescKeyPrev | wrist-shot-on-goal-for | typeDescKeyPrev_wrist.shot.on.goal.for |
| typeDescKeyPrev | unknown | typeDescKeyPrev_unknown |
| typeDescKeyPrev | new | typeDescKeyPrev_new |
| shotType | deflected | shotType_deflected |
| shotType | other | shotType_other |
| shotType | slap | shotType_slap |
| shotType | snap | shotType_snap |
| shotType | tip-in | shotType_tip.in |
| shotType | wrist | shotType_wrist |
| shotType | unknown | shotType_unknown |
| shotType | new | shotType_new |
| isRebound | yes | isRebound_yes |
| isRebound | unknown | isRebound_unknown |
| isRebound | new | isRebound_new |
| isRush | yes | isRush_yes |
| isRush | unknown | isRush_unknown |
| isRush | new | isRush_new |
| shooterHandCode | R | shooterHandCode_R |
| shooterHandCode | unknown | shooterHandCode_unknown |
| shooterHandCode | new | shooterHandCode_new |
| shooterPositionCode | D | shooterPositionCode_D |
| shooterPositionCode | L | shooterPositionCode_L |
| shooterPositionCode | R | shooterPositionCode_R |
| shooterPositionCode | unknown | shooterPositionCode_unknown |
| shooterPositionCode | new | shooterPositionCode_new |
| goalieHandCode | R | goalieHandCode_R |
| goalieHandCode | unknown | goalieHandCode_unknown |
| goalieHandCode | new | goalieHandCode_new |
| isEmptyNetFor | yes | isEmptyNetFor_yes |
| isEmptyNetFor | unknown | isEmptyNetFor_unknown |
| isEmptyNetFor | new | isEmptyNetFor_new |
| strengthState | power-play | strengthState_power.play |
| strengthState | unknown | strengthState_unknown |
| strengthState | new | strengthState_new |

Unknown categorical fills:

| variable | value |
| --- | --- |
| isPlayoff | unknown |
| isHome | unknown |
| isOvertime | unknown |
| zoneCode | unknown |
| isBehindNet | unknown |
| crossedRoyalRoad | unknown |
| typeDescKeyPrev | unknown |
| shotType | unknown |
| isRebound | unknown |
| isRush | unknown |
| shooterHandCode | unknown |
| shooterPositionCode | unknown |
| goalieHandCode | unknown |
| isEmptyNetFor | unknown |
| strengthState | unknown |

Novel categorical buckets:

| variable | value |
| --- | --- |
| isPlayoff | new |
| isHome | new |
| isOvertime | new |
| zoneCode | new |
| isBehindNet | new |
| crossedRoyalRoad | new |
| typeDescKeyPrev | new |
| shotType | new |
| isRebound | new |
| isRush | new |
| shooterHandCode | new |
| shooterPositionCode | new |
| goalieHandCode | new |
| isEmptyNetFor | new |
| strengthState | new |

Zero-variance terms removed before fitting:

| term |
| --- |
| isPlayoff_unknown |
| isPlayoff_new |
| isHome_unknown |
| isHome_new |
| isOvertime_unknown |
| isOvertime_new |
| zoneCode_unknown |
| zoneCode_new |
| isBehindNet_unknown |
| isBehindNet_new |
| crossedRoyalRoad_unknown |
| crossedRoyalRoad_new |
| typeDescKeyPrev_unknown |
| typeDescKeyPrev_new |
| shotType_unknown |
| shotType_new |
| isRebound_unknown |
| isRebound_new |
| isRush_unknown |
| isRush_new |
| shooterHandCode_unknown |
| shooterHandCode_new |
| shooterPositionCode_unknown |
| shooterPositionCode_new |
| goalieHandCode_unknown |
| goalieHandCode_new |
| isEmptyNetFor_unknown |
| isEmptyNetFor_new |
| strengthState_unknown |
| strengthState_new |

## SH: Short-Handed

- Raw predictor columns: `isPlayoff`, `isHome`, `isOvertime`, `periodNumber`, `secondsElapsedInPeriod`, `secondsElapsedInGame`, `secondsElapsedInSequence`, `zoneCode`, `xCoordNorm`, `yCoordNorm`, `dXCoordNorm`, `dYCoordNorm`, `distance`, `angle`, `dDistance`, `dAngle`, `dSecondsElapsedInSequence`, `dXCoordNormPerSecond`, `dYCoordNormPerSecond`, `dDistancePerSecond`, `dAnglePerSecond`, `isBehindNet`, `crossedRoyalRoad`, `typeDescKeyPrev`, `shotType`, `isRebound`, `isRush`, `goalsFor`, `goalsAgainst`, `goalDifferential`, `shotsFor`, `shotsAgainst`, `shotDifferential`, `fenwickFor`, `fenwickAgainst`, `fenwickDifferential`, `corsiFor`, `corsiAgainst`, `corsiDifferential`, `shooterHeight`, `shooterWeight`, `shooterHandCode`, `shooterPositionCode`, `shooterAge`, `shooterSecondsElapsedInShift`, `shooterSecondsElapsedSinceLastShift`, `goalieHeight`, `goalieWeight`, `goalieHandCode`, `goalieAge`, `minSecondsElapsedInShiftFor`, `maxSecondsElapsedInShiftFor`, `avgSecondsElapsedInShiftFor`, `minSecondsElapsedInShiftAgainst`, `maxSecondsElapsedInShiftAgainst`, `avgSecondsElapsedInShiftAgainst`, `minSecondsElapsedSinceLastShiftFor`, `maxSecondsElapsedSinceLastShiftFor`, `avgSecondsElapsedSinceLastShiftFor`, `minSecondsElapsedSinceLastShiftAgainst`, `maxSecondsElapsedSinceLastShiftAgainst`, `avgSecondsElapsedSinceLastShiftAgainst`, `isEmptyNetFor`, `skaterCountFor`, `skaterCountAgainst`, `manDifferential`, `strengthState`
- Best penalty (`glmnet` lambda): `0.0000001`
- Final searched log10 range: `[-7, 2]`
- Search note: `accepted_lower_boundary`

Training summary:

| seasons | games | rows | goal_rate |
| --- | --- | --- | --- |
| 2023,2024 | 2241 | 5539 | 0.0738400433291208 |

Best-penalty grouped CV metrics:

| .metric | mean | std_err |
| --- | --- | --- |
| mn_log_loss | 0.221051990106459 | 0.00652395306295094 |
| roc_auc | 0.796020292767421 | 0.0166689854069211 |
| pr_auc | 0.980318920925609 | 0.00202937680750877 |
| brier_class | 0.0627852175246014 | 0.00173659761119842 |

Top grouped CV log-loss settings:

| penalty | .metric | .estimator | mean | n | std_err | .config |
| --- | --- | --- | --- | --- | --- | --- |
| 0.0000001 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod01_post0 |
| 0.000000204335971785694 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod02_post0 |
| 0.00000041753189365604 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod03_post0 |
| 0.000000853167852417281 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod04_post0 |
| 0.00000174332882219999 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod05_post0 |
| 0.00000356224789026244 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod06_post0 |
| 0.00000727895384398316 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod07_post0 |
| 0.0000148735210729351 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod08_post0 |
| 0.000030391953823132 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod09_post0 |
| 0.0000621016941891562 | mn_log_loss | binary | 0.221051990106459 | 5 | 0.00652395306295094 | pre0_mod10_post0 |

Penalty search history:

| iteration | log10_lower | log10_upper | penalty_lower | penalty_upper | best_penalty | best_cv_mn_log_loss | boundary_hit |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | -7 | 2 | 0.0000001 | 100 | 0.0000001 | 0.221051990106459 | accepted_lower_boundary |

Coefficients:

| term | estimate |
| --- | --- |
| (Intercept) | -3.6619659531882 |
| distance | -0.970687551707679 |
| xCoordNorm | 0.717455294197871 |
| angle | -0.297024175456082 |
| minSecondsElapsedInShiftFor | 0.27122380087871 |
| dDistance | -0.267518358377041 |
| zoneCode_O | 0.195159633448741 |
| zoneCode_N | -0.190827028423804 |
| shotType_tip.in | -0.186855895828174 |
| shotType_snap | 0.183604349660997 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | -0.160395450910082 |
| typeDescKeyPrev_won.faceoff | -0.151129556544712 |
| typeDescKeyPrev_post.missed.shot.against | -0.147241257669834 |
| avgSecondsElapsedInShiftAgainst | 0.144211138199073 |
| secondsElapsedInGame | -0.121661223926059 |
| corsiDifferential | -0.118990850682781 |
| isOvertime_yes | -0.117737526102611 |
| minSecondsElapsedSinceLastShiftFor | 0.116084350907325 |
| avgSecondsElapsedSinceLastShiftFor | -0.115716271274821 |
| goalieHeight | 0.11483098562009 |
| dXCoordNorm | 0.112023132237177 |
| fenwickDifferential | 0.110977953784984 |
| shooterHandCode_R | 0.103862802261297 |
| shooterPositionCode_D | -0.103113633941836 |
| typeDescKeyPrev_blocked.shot.for | 0.100800767833885 |
| dDistancePerSecond | 0.0975435443062136 |
| typeDescKeyPrev_takeaway.for | 0.0972548075319404 |
| periodNumber | -0.0948859999673318 |
| shooterSecondsElapsedSinceLastShift | 0.0947505972258885 |
| typeDescKeyPrev_lost.faceoff | 0.0945520719338904 |
| manDifferential | 0.0936133945867194 |
| shooterWeight | 0.092700498818122 |
| shooterHeight | -0.0923304980084952 |
| typeDescKeyPrev_post.missed.shot.for | 0.0903659157101256 |
| dXCoordNormPerSecond | -0.0877809211979315 |
| dAngle | -0.0875968942025955 |
| goalieAge | 0.0866086734892272 |
| corsiAgainst | 0.0842431605176304 |
| goalieWeight | -0.0802368087695906 |
| dYCoordNormPerSecond | 0.0785095730199078 |
| secondsElapsedInPeriod | -0.0779119292513065 |
| shooterSecondsElapsedInShift | -0.0776804624636681 |
| maxSecondsElapsedSinceLastShiftAgainst | 0.0669135815904989 |
| dSecondsElapsedInSequence | -0.0664507452052154 |
| shooterPositionCode_R | 0.0627470359510904 |
| typeDescKeyPrev_deflected.shot.on.goal.against | -0.0605555228451439 |
| shotType_other | -0.0589976639194994 |
| isRebound_yes | 0.0585776002764656 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0.0576091188259253 |
| typeDescKeyPrev_other.shot.on.goal.for | -0.0565658645943384 |
| avgSecondsElapsedInShiftFor | 0.0552186680889586 |
| fenwickFor | 0.0525046080514193 |
| typeDescKeyPrev_high.missed.shot.for | -0.0524749839813747 |
| dYCoordNorm | -0.0510900342913744 |
| crossedRoyalRoad_yes | 0.0493371850377174 |
| maxSecondsElapsedInShiftFor | 0.0487753317279111 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0.0483203852189633 |
| skaterCountAgainst | -0.0456071290828639 |
| typeDescKeyPrev_deflected.shot.on.goal.for | -0.0445083352877965 |
| typeDescKeyPrev_giveaway.against | 0.0422844013409392 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0.0391139059829032 |
| shotType_slap | 0.0383609837965214 |
| typeDescKeyPrev_other.missed.shot.for | -0.0373681168196171 |
| maxSecondsElapsedSinceLastShiftFor | -0.0364220938550834 |
| shooterPositionCode_L | -0.0360581352007317 |
| shotsFor | 0.0340941328562104 |
| typeDescKeyPrev_giveaway.for | -0.0324786454023091 |
| fenwickAgainst | -0.0320365084571158 |
| isHome_yes | -0.0319924528017952 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0.0313425322361808 |
| typeDescKeyPrev_other.shot.on.goal.against | 0.031029335280658 |
| typeDescKeyPrev_taken.hit | -0.0310041322365639 |
| isBehindNet_yes | 0.0306430051517611 |
| typeDescKeyPrev_wrist.shot.on.goal.against | -0.0306079595687004 |
| minSecondsElapsedSinceLastShiftAgainst | -0.0303087523625965 |
| typeDescKeyPrev_other.missed.shot.against | 0.0288797269564314 |
| skaterCountFor | 0.0286272113343239 |
| isRush_yes | -0.0279858976473977 |
| typeDescKeyPrev_snap.shot.on.goal.for | -0.0277729041320115 |
| avgSecondsElapsedSinceLastShiftAgainst | -0.025916298872112 |
| yCoordNorm | 0.0247877846769411 |
| shotDifferential | 0.0233057395006472 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0.0230022410453745 |
| secondsElapsedInSequence | -0.0229531879315887 |
| maxSecondsElapsedInShiftAgainst | -0.0226537769568283 |
| goalsFor | 0.0214043673511053 |
| shotType_deflected | -0.0211134373905822 |
| minSecondsElapsedInShiftAgainst | -0.0206195382917677 |
| shooterAge | 0.0204977010347946 |
| typeDescKeyPrev_wide.missed.shot.against | 0.0199426170371812 |
| shotType_wrist | -0.0186361843569542 |
| typeDescKeyPrev_blocked.shot.against | 0.0172834127630525 |
| shotsAgainst | 0.0170657257502083 |
| dAnglePerSecond | 0.0168855503164496 |
| isEmptyNetFor_yes | -0.015215228791608 |
| goalsAgainst | 0.0150503650562942 |
| typeDescKeyPrev_high.missed.shot.against | 0.0150058420989662 |
| goalieHandCode_R | -0.0119434658430105 |
| typeDescKeyPrev_given.hit | -0.00855876204316625 |
| isPlayoff_yes | 0.00634373874355267 |
| goalDifferential | 0.00556229455220673 |
| corsiFor | -0.0046545192431619 |
| typeDescKeyPrev_wide.missed.shot.for | -0.00157276964166773 |
| typeDescKeyPrev_takeaway.against | -0.00134965996547564 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0.000658526327013083 |

Numeric imputation medians:

| term | median |
| --- | --- |
| periodNumber | 2 |
| secondsElapsedInPeriod | 617 |
| secondsElapsedInGame | 1816 |
| secondsElapsedInSequence | 38 |
| xCoordNorm | 59 |
| yCoordNorm | 1 |
| dXCoordNorm | 7 |
| dYCoordNorm | 1 |
| distance | 36.1247837363769 |
| angle | 21.8014094863518 |
| dDistance | -8.42454201792223 |
| dAngle | 1.48040918951731 |
| dSecondsElapsedInSequence | 9 |
| dXCoordNormPerSecond | 0.454545454545455 |
| dYCoordNormPerSecond | 0.0526315789473684 |
| dDistancePerSecond | -0.571766541280781 |
| dAnglePerSecond | 0.113565361423721 |
| goalsFor | 1 |
| goalsAgainst | 1 |
| goalDifferential | 0 |
| shotsFor | 14 |
| shotsAgainst | 14 |
| shotDifferential | 0 |
| fenwickFor | 21 |
| fenwickAgainst | 21 |
| fenwickDifferential | 0 |
| corsiFor | 29 |
| corsiAgainst | 29 |
| corsiDifferential | 0 |
| shooterHeight | 73 |
| shooterWeight | 201 |
| shooterAge | 28 |
| shooterSecondsElapsedInShift | 24 |
| shooterSecondsElapsedSinceLastShift | 113 |
| goalieHeight | 75 |
| goalieWeight | 201 |
| goalieAge | 28 |
| minSecondsElapsedInShiftFor | 12 |
| maxSecondsElapsedInShiftFor | 35 |
| avgSecondsElapsedInShiftFor | 23.75 |
| minSecondsElapsedInShiftAgainst | 24 |
| maxSecondsElapsedInShiftAgainst | 57 |
| avgSecondsElapsedInShiftAgainst | 40.4 |
| minSecondsElapsedSinceLastShiftFor | 62 |
| maxSecondsElapsedSinceLastShiftFor | 183 |
| avgSecondsElapsedSinceLastShiftFor | 119.5 |
| minSecondsElapsedSinceLastShiftAgainst | 89 |
| maxSecondsElapsedSinceLastShiftAgainst | 220 |
| avgSecondsElapsedSinceLastShiftAgainst | 150.6 |
| skaterCountFor | 4 |
| skaterCountAgainst | 5 |
| manDifferential | -1 |
| isPlayoff_yes | 0 |
| isPlayoff_unknown | 0 |
| isPlayoff_new | 0 |
| isHome_yes | 0 |
| isHome_unknown | 0 |
| isHome_new | 0 |
| isOvertime_yes | 0 |
| isOvertime_unknown | 0 |
| isOvertime_new | 0 |
| zoneCode_N | 0 |
| zoneCode_O | 1 |
| zoneCode_unknown | 0 |
| zoneCode_new | 0 |
| isBehindNet_yes | 0 |
| isBehindNet_unknown | 0 |
| isBehindNet_new | 0 |
| crossedRoyalRoad_yes | 0 |
| crossedRoyalRoad_unknown | 0 |
| crossedRoyalRoad_new | 0 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0 |
| typeDescKeyPrev_blocked.shot.against | 0 |
| typeDescKeyPrev_blocked.shot.for | 0 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0 |
| typeDescKeyPrev_deflected.shot.on.goal.for | 0 |
| typeDescKeyPrev_giveaway.against | 0 |
| typeDescKeyPrev_giveaway.for | 0 |
| typeDescKeyPrev_given.hit | 0 |
| typeDescKeyPrev_high.missed.shot.against | 0 |
| typeDescKeyPrev_high.missed.shot.for | 0 |
| typeDescKeyPrev_lost.faceoff | 0 |
| typeDescKeyPrev_other.missed.shot.against | 0 |
| typeDescKeyPrev_other.missed.shot.for | 0 |
| typeDescKeyPrev_other.shot.on.goal.against | 0 |
| typeDescKeyPrev_other.shot.on.goal.for | 0 |
| typeDescKeyPrev_post.missed.shot.against | 0 |
| typeDescKeyPrev_post.missed.shot.for | 0 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0 |
| typeDescKeyPrev_takeaway.against | 0 |
| typeDescKeyPrev_takeaway.for | 0 |
| typeDescKeyPrev_taken.hit | 0 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0 |
| typeDescKeyPrev_wide.missed.shot.against | 0 |
| typeDescKeyPrev_wide.missed.shot.for | 0 |
| typeDescKeyPrev_won.faceoff | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0 |
| typeDescKeyPrev_unknown | 0 |
| typeDescKeyPrev_new | 0 |
| shotType_deflected | 0 |
| shotType_other | 0 |
| shotType_slap | 0 |
| shotType_snap | 0 |
| shotType_tip.in | 0 |
| shotType_wrist | 1 |
| shotType_unknown | 0 |
| shotType_new | 0 |
| isRebound_yes | 0 |
| isRebound_unknown | 0 |
| isRebound_new | 0 |
| isRush_yes | 0 |
| isRush_unknown | 0 |
| isRush_new | 0 |
| shooterHandCode_R | 0 |
| shooterHandCode_unknown | 0 |
| shooterHandCode_new | 0 |
| shooterPositionCode_D | 0 |
| shooterPositionCode_L | 0 |
| shooterPositionCode_R | 0 |
| shooterPositionCode_unknown | 0 |
| shooterPositionCode_new | 0 |
| goalieHandCode_R | 0 |
| goalieHandCode_unknown | 0 |
| goalieHandCode_new | 0 |
| isEmptyNetFor_yes | 0 |
| isEmptyNetFor_unknown | 0 |
| isEmptyNetFor_new | 0 |
| strengthState_unknown | 0 |
| strengthState_new | 0 |

Numeric normalization parameters:

| term | mean | sd |
| --- | --- | --- |
| periodNumber | 2.002166456039 | 0.779149326677817 |
| secondsElapsedInPeriod | 616.502437263044 | 337.639737591807 |
| secondsElapsedInGame | 1819.10218450984 | 938.32721291578 |
| secondsElapsedInSequence | 43.9142444484564 | 32.5207777738987 |
| xCoordNorm | 30.2457122224228 | 59.9419795694989 |
| yCoordNorm | 0.929048564722874 | 20.4192740476686 |
| dXCoordNorm | 14.5161581512908 | 79.2422780133583 |
| dYCoordNorm | 0.958115183246073 | 29.5352721497719 |
| distance | 63.3186071116889 | 58.7670883220526 |
| angle | 25.9586931646104 | 21.0766263271897 |
| dDistance | -15.0776572060935 | 76.954308076194 |
| dAngle | 1.07370767403301 | 30.7767583816096 |
| dSecondsElapsedInSequence | 15.4733706445207 | 16.3530855178288 |
| dXCoordNormPerSecond | 0.976687285593546 | 30.9173379907776 |
| dYCoordNormPerSecond | 0.193272905424111 | 10.3834931973101 |
| dDistancePerSecond | -1.23108168471295 | 29.5582556005456 |
| dAnglePerSecond | 0.00966465385158028 | 12.9647604387722 |
| goalsFor | 1.3702834446651 | 1.41444009515148 |
| goalsAgainst | 1.3451886622134 | 1.38544771847055 |
| goalDifferential | 0.0250947824517061 | 1.71642122168878 |
| shotsFor | 14.6766564361798 | 9.02796397666486 |
| shotsAgainst | 14.7402058133237 | 8.83136376475086 |
| shotDifferential | -0.0635493771438888 | 7.02499832113333 |
| fenwickFor | 21.4045856652825 | 12.8833021143722 |
| fenwickAgainst | 21.5414334717458 | 12.4757709639324 |
| fenwickDifferential | -0.136847806463261 | 9.57438677685876 |
| corsiFor | 29.8887885899982 | 17.7345270978743 |
| corsiAgainst | 30 | 17.0081555862583 |
| corsiDifferential | -0.111211410001805 | 12.7887769460849 |
| shooterHeight | 73.4625383643257 | 2.09183250277877 |
| shooterWeight | 201.888788589998 | 14.9907488853502 |
| shooterAge | 28.4090991153638 | 3.8308629926472 |
| shooterSecondsElapsedInShift | 28.0164289582957 | 19.4483764677392 |
| shooterSecondsElapsedSinceLastShift | 144.614551363062 | 103.502442094525 |
| goalieHeight | 74.9333814768009 | 1.63246329107357 |
| goalieWeight | 201.911716916411 | 15.4357662596925 |
| goalieAge | 28.5629174941325 | 3.69412931750678 |
| minSecondsElapsedInShiftFor | 14.009387976169 | 12.8245058716988 |
| maxSecondsElapsedInShiftFor | 39.9857374977433 | 25.9381082432472 |
| avgSecondsElapsedInShiftFor | 25.5880724559186 | 14.0922333768974 |
| minSecondsElapsedInShiftAgainst | 28.8573749774327 | 23.6603331047493 |
| maxSecondsElapsedInShiftAgainst | 62.3211771077812 | 36.6052498517267 |
| avgSecondsElapsedInShiftAgainst | 43.4658482277186 | 25.1271231971324 |
| minSecondsElapsedSinceLastShiftFor | 81.1953421195162 | 70.8351060553903 |
| maxSecondsElapsedSinceLastShiftFor | 208.162845278931 | 110.485841551667 |
| avgSecondsElapsedSinceLastShiftFor | 137.555109225492 | 73.7275768517659 |
| minSecondsElapsedSinceLastShiftAgainst | 105.861527351507 | 77.0793009396656 |
| maxSecondsElapsedSinceLastShiftAgainst | 243.948185593067 | 110.086985969387 |
| avgSecondsElapsedSinceLastShiftAgainst | 168.094042245893 | 76.1252366178611 |
| skaterCountFor | 3.99115363784077 | 0.0936466085750869 |
| skaterCountAgainst | 4.99512547391226 | 0.0696537220020474 |
| manDifferential | -1.00433291207799 | 0.0656880275043174 |
| isPlayoff_yes | 0.0610218450983932 | 0.239391992109196 |
| isHome_yes | 0.486549918757899 | 0.499864186854074 |
| isOvertime_yes | 0.00361076006499368 | 0.0599864327966236 |
| zoneCode_N | 0.0507311789131612 | 0.219468043762246 |
| zoneCode_O | 0.710236504784257 | 0.453693479782045 |
| isBehindNet_yes | 0.0111933562014804 | 0.105214369482472 |
| crossedRoyalRoad_yes | 0.437263043870735 | 0.496093243527819 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0.00686044412348799 | 0.0825506434000914 |
| typeDescKeyPrev_blocked.shot.against | 0.0949629897093338 | 0.293190278554627 |
| typeDescKeyPrev_blocked.shot.for | 0.0110128181982307 | 0.104371944145902 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0.00126376602274778 | 0.0355302241590221 |
| typeDescKeyPrev_deflected.shot.on.goal.for | 0.000541614009749052 | 0.0232683993994501 |
| typeDescKeyPrev_giveaway.against | 0.128001444304026 | 0.3341215786545 |
| typeDescKeyPrev_giveaway.for | 0.0146235782632244 | 0.120051368986364 |
| typeDescKeyPrev_given.hit | 0.0478425708611663 | 0.213452301290697 |
| typeDescKeyPrev_high.missed.shot.against | 0.00397183607149305 | 0.0629029008707939 |
| typeDescKeyPrev_high.missed.shot.for | 0.00090269001624842 | 0.0300339477779027 |
| typeDescKeyPrev_lost.faceoff | 0.105795269904315 | 0.307603174925572 |
| typeDescKeyPrev_other.missed.shot.against | 0.0045134500812421 | 0.0670364838544191 |
| typeDescKeyPrev_other.missed.shot.for | 0.000361076006499368 | 0.0190002843870062 |
| typeDescKeyPrev_other.shot.on.goal.against | 0.00198591803574652 | 0.0445233876919983 |
| typeDescKeyPrev_other.shot.on.goal.for | 0.000541614009749052 | 0.0232683993994487 |
| typeDescKeyPrev_post.missed.shot.against | 0.00848528615273515 | 0.0917322476764969 |
| typeDescKeyPrev_post.missed.shot.for | 0.00288860805199494 | 0.0536729362577081 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0.0198591803574652 | 0.139528879026752 |
| typeDescKeyPrev_slap.shot.on.goal.for | 0.0021664560389962 | 0.0464989554504953 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0.017692724318469 | 0.131843961128651 |
| typeDescKeyPrev_snap.shot.on.goal.for | 0.00758259613648673 | 0.0867551680548767 |
| typeDescKeyPrev_takeaway.against | 0.0110128181982307 | 0.104371944145901 |
| typeDescKeyPrev_takeaway.for | 0.0935186856833363 | 0.291184217656941 |
| typeDescKeyPrev_taken.hit | 0.0216645603899621 | 0.145598882016394 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0.00956851417223325 | 0.0973584560720779 |
| typeDescKeyPrev_tip.in.shot.on.goal.for | 0.00252753204549557 | 0.0502155242099449 |
| typeDescKeyPrev_wide.missed.shot.against | 0.0527170969489077 | 0.223488303885924 |
| typeDescKeyPrev_wide.missed.shot.for | 0.01715111030872 | 0.129846038035338 |
| typeDescKeyPrev_won.faceoff | 0.217728831919119 | 0.412739316027509 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0.0440512727929229 | 0.205227586165316 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0.0436901967864235 | 0.204423354774323 |
| shotType_deflected | 0.00704098212673767 | 0.083622180908848 |
| shotType_other | 0.0111933562014804 | 0.105214369482471 |
| shotType_slap | 0.0630077631341397 | 0.242998858860298 |
| shotType_snap | 0.152554612745983 | 0.359590110196277 |
| shotType_tip.in | 0.0225672504062105 | 0.148532665216068 |
| shotType_wrist | 0.638201841487633 | 0.480564194262334 |
| isRebound_yes | 0.0462177288319191 | 0.20997526098028 |
| isRush_yes | 0.0844917855208521 | 0.278149045233744 |
| shooterHandCode_R | 0.364867304567611 | 0.481436392376066 |
| shooterPositionCode_D | 0.267376782812782 | 0.442630557189468 |
| shooterPositionCode_L | 0.171872179093699 | 0.377303371501509 |
| shooterPositionCode_R | 0.144430402599747 | 0.351557356005032 |
| goalieHandCode_R | 0.0575916230366492 | 0.232990618761944 |
| isEmptyNetFor_yes | 0.000361076006499368 | 0.0190002843870071 |

Dummy map:

| variable | level | output_column |
| --- | --- | --- |
| isPlayoff | yes | isPlayoff_yes |
| isPlayoff | unknown | isPlayoff_unknown |
| isPlayoff | new | isPlayoff_new |
| isHome | yes | isHome_yes |
| isHome | unknown | isHome_unknown |
| isHome | new | isHome_new |
| isOvertime | yes | isOvertime_yes |
| isOvertime | unknown | isOvertime_unknown |
| isOvertime | new | isOvertime_new |
| zoneCode | N | zoneCode_N |
| zoneCode | O | zoneCode_O |
| zoneCode | unknown | zoneCode_unknown |
| zoneCode | new | zoneCode_new |
| isBehindNet | yes | isBehindNet_yes |
| isBehindNet | unknown | isBehindNet_unknown |
| isBehindNet | new | isBehindNet_new |
| crossedRoyalRoad | yes | crossedRoyalRoad_yes |
| crossedRoyalRoad | unknown | crossedRoyalRoad_unknown |
| crossedRoyalRoad | new | crossedRoyalRoad_new |
| typeDescKeyPrev | backhand-shot-on-goal-for | typeDescKeyPrev_backhand.shot.on.goal.for |
| typeDescKeyPrev | blocked-shot-against | typeDescKeyPrev_blocked.shot.against |
| typeDescKeyPrev | blocked-shot-for | typeDescKeyPrev_blocked.shot.for |
| typeDescKeyPrev | deflected-shot-on-goal-against | typeDescKeyPrev_deflected.shot.on.goal.against |
| typeDescKeyPrev | deflected-shot-on-goal-for | typeDescKeyPrev_deflected.shot.on.goal.for |
| typeDescKeyPrev | giveaway-against | typeDescKeyPrev_giveaway.against |
| typeDescKeyPrev | giveaway-for | typeDescKeyPrev_giveaway.for |
| typeDescKeyPrev | given-hit | typeDescKeyPrev_given.hit |
| typeDescKeyPrev | high-missed-shot-against | typeDescKeyPrev_high.missed.shot.against |
| typeDescKeyPrev | high-missed-shot-for | typeDescKeyPrev_high.missed.shot.for |
| typeDescKeyPrev | lost-faceoff | typeDescKeyPrev_lost.faceoff |
| typeDescKeyPrev | other-missed-shot-against | typeDescKeyPrev_other.missed.shot.against |
| typeDescKeyPrev | other-missed-shot-for | typeDescKeyPrev_other.missed.shot.for |
| typeDescKeyPrev | other-shot-on-goal-against | typeDescKeyPrev_other.shot.on.goal.against |
| typeDescKeyPrev | other-shot-on-goal-for | typeDescKeyPrev_other.shot.on.goal.for |
| typeDescKeyPrev | post-missed-shot-against | typeDescKeyPrev_post.missed.shot.against |
| typeDescKeyPrev | post-missed-shot-for | typeDescKeyPrev_post.missed.shot.for |
| typeDescKeyPrev | slap-shot-on-goal-against | typeDescKeyPrev_slap.shot.on.goal.against |
| typeDescKeyPrev | slap-shot-on-goal-for | typeDescKeyPrev_slap.shot.on.goal.for |
| typeDescKeyPrev | snap-shot-on-goal-against | typeDescKeyPrev_snap.shot.on.goal.against |
| typeDescKeyPrev | snap-shot-on-goal-for | typeDescKeyPrev_snap.shot.on.goal.for |
| typeDescKeyPrev | takeaway-against | typeDescKeyPrev_takeaway.against |
| typeDescKeyPrev | takeaway-for | typeDescKeyPrev_takeaway.for |
| typeDescKeyPrev | taken-hit | typeDescKeyPrev_taken.hit |
| typeDescKeyPrev | tip-in-shot-on-goal-against | typeDescKeyPrev_tip.in.shot.on.goal.against |
| typeDescKeyPrev | tip-in-shot-on-goal-for | typeDescKeyPrev_tip.in.shot.on.goal.for |
| typeDescKeyPrev | wide-missed-shot-against | typeDescKeyPrev_wide.missed.shot.against |
| typeDescKeyPrev | wide-missed-shot-for | typeDescKeyPrev_wide.missed.shot.for |
| typeDescKeyPrev | won-faceoff | typeDescKeyPrev_won.faceoff |
| typeDescKeyPrev | wrist-shot-on-goal-against | typeDescKeyPrev_wrist.shot.on.goal.against |
| typeDescKeyPrev | wrist-shot-on-goal-for | typeDescKeyPrev_wrist.shot.on.goal.for |
| typeDescKeyPrev | unknown | typeDescKeyPrev_unknown |
| typeDescKeyPrev | new | typeDescKeyPrev_new |
| shotType | deflected | shotType_deflected |
| shotType | other | shotType_other |
| shotType | slap | shotType_slap |
| shotType | snap | shotType_snap |
| shotType | tip-in | shotType_tip.in |
| shotType | wrist | shotType_wrist |
| shotType | unknown | shotType_unknown |
| shotType | new | shotType_new |
| isRebound | yes | isRebound_yes |
| isRebound | unknown | isRebound_unknown |
| isRebound | new | isRebound_new |
| isRush | yes | isRush_yes |
| isRush | unknown | isRush_unknown |
| isRush | new | isRush_new |
| shooterHandCode | R | shooterHandCode_R |
| shooterHandCode | unknown | shooterHandCode_unknown |
| shooterHandCode | new | shooterHandCode_new |
| shooterPositionCode | D | shooterPositionCode_D |
| shooterPositionCode | L | shooterPositionCode_L |
| shooterPositionCode | R | shooterPositionCode_R |
| shooterPositionCode | unknown | shooterPositionCode_unknown |
| shooterPositionCode | new | shooterPositionCode_new |
| goalieHandCode | R | goalieHandCode_R |
| goalieHandCode | unknown | goalieHandCode_unknown |
| goalieHandCode | new | goalieHandCode_new |
| isEmptyNetFor | yes | isEmptyNetFor_yes |
| isEmptyNetFor | unknown | isEmptyNetFor_unknown |
| isEmptyNetFor | new | isEmptyNetFor_new |
| strengthState | unknown | strengthState_unknown |
| strengthState | new | strengthState_new |

Unknown categorical fills:

| variable | value |
| --- | --- |
| isPlayoff | unknown |
| isHome | unknown |
| isOvertime | unknown |
| zoneCode | unknown |
| isBehindNet | unknown |
| crossedRoyalRoad | unknown |
| typeDescKeyPrev | unknown |
| shotType | unknown |
| isRebound | unknown |
| isRush | unknown |
| shooterHandCode | unknown |
| shooterPositionCode | unknown |
| goalieHandCode | unknown |
| isEmptyNetFor | unknown |
| strengthState | unknown |

Novel categorical buckets:

| variable | value |
| --- | --- |
| isPlayoff | new |
| isHome | new |
| isOvertime | new |
| zoneCode | new |
| isBehindNet | new |
| crossedRoyalRoad | new |
| typeDescKeyPrev | new |
| shotType | new |
| isRebound | new |
| isRush | new |
| shooterHandCode | new |
| shooterPositionCode | new |
| goalieHandCode | new |
| isEmptyNetFor | new |
| strengthState | new |

Zero-variance terms removed before fitting:

| term |
| --- |
| isPlayoff_unknown |
| isPlayoff_new |
| isHome_unknown |
| isHome_new |
| isOvertime_unknown |
| isOvertime_new |
| zoneCode_unknown |
| zoneCode_new |
| isBehindNet_unknown |
| isBehindNet_new |
| crossedRoyalRoad_unknown |
| crossedRoyalRoad_new |
| typeDescKeyPrev_unknown |
| typeDescKeyPrev_new |
| shotType_unknown |
| shotType_new |
| isRebound_unknown |
| isRebound_new |
| isRush_unknown |
| isRush_new |
| shooterHandCode_unknown |
| shooterHandCode_new |
| shooterPositionCode_unknown |
| shooterPositionCode_new |
| goalieHandCode_unknown |
| goalieHandCode_new |
| isEmptyNetFor_unknown |
| isEmptyNetFor_new |
| strengthState_unknown |
| strengthState_new |

## EN: Empty Net Against

- Raw predictor columns: `isPlayoff`, `isHome`, `isOvertime`, `periodNumber`, `secondsElapsedInPeriod`, `secondsElapsedInGame`, `secondsElapsedInSequence`, `zoneCode`, `xCoordNorm`, `yCoordNorm`, `dXCoordNorm`, `dYCoordNorm`, `distance`, `angle`, `dDistance`, `dAngle`, `dSecondsElapsedInSequence`, `dXCoordNormPerSecond`, `dYCoordNormPerSecond`, `dDistancePerSecond`, `dAnglePerSecond`, `isBehindNet`, `crossedRoyalRoad`, `typeDescKeyPrev`, `shotType`, `isRebound`, `isRush`, `goalsFor`, `goalsAgainst`, `goalDifferential`, `shotsFor`, `shotsAgainst`, `shotDifferential`, `fenwickFor`, `fenwickAgainst`, `fenwickDifferential`, `corsiFor`, `corsiAgainst`, `corsiDifferential`, `shooterHeight`, `shooterWeight`, `shooterHandCode`, `shooterPositionCode`, `shooterAge`, `shooterSecondsElapsedInShift`, `shooterSecondsElapsedSinceLastShift`, `minSecondsElapsedInShiftFor`, `maxSecondsElapsedInShiftFor`, `avgSecondsElapsedInShiftFor`, `minSecondsElapsedInShiftAgainst`, `maxSecondsElapsedInShiftAgainst`, `avgSecondsElapsedInShiftAgainst`, `minSecondsElapsedSinceLastShiftFor`, `maxSecondsElapsedSinceLastShiftFor`, `avgSecondsElapsedSinceLastShiftFor`, `minSecondsElapsedSinceLastShiftAgainst`, `maxSecondsElapsedSinceLastShiftAgainst`, `avgSecondsElapsedSinceLastShiftAgainst`, `isEmptyNetFor`, `skaterCountFor`, `skaterCountAgainst`, `manDifferential`, `strengthState`
- Best penalty (`glmnet` lambda): `0.0788046281566992`
- Final searched log10 range: `[-7, 2]`
- Search note: `none`

Training summary:

| seasons | games | rows | goal_rate |
| --- | --- | --- | --- |
| 2023,2024 | 1245 | 1828 | 0.573851203501094 |

Best-penalty grouped CV metrics:

| .metric | mean | std_err |
| --- | --- | --- |
| mn_log_loss | 0.619050049619958 | 0.010254898414506 |
| roc_auc | 0.700180975003345 | 0.014806440407232 |
| pr_auc | 0.599215259469518 | 0.0139539768102888 |
| brier_class | 0.216064913970363 | 0.00464949024639235 |

Top grouped CV log-loss settings:

| penalty | .metric | .estimator | mean | n | std_err | .config |
| --- | --- | --- | --- | --- | --- | --- |
| 0.0788046281566992 | mn_log_loss | binary | 0.619050049619958 | 5 | 0.010254898414506 | pre0_mod20_post0 |
| 0.161026202756094 | mn_log_loss | binary | 0.620101920325864 | 5 | 0.00893401370199361 | pre0_mod21_post0 |
| 0.0385662042116347 | mn_log_loss | binary | 0.621055577636467 | 5 | 0.0111914104141593 | pre0_mod19_post0 |
| 0.329034456231267 | mn_log_loss | binary | 0.624675560645477 | 5 | 0.00740179864007703 | pre0_mod22_post0 |
| 0.018873918221351 | mn_log_loss | binary | 0.624875719274407 | 5 | 0.0117005720225556 | pre0_mod18_post0 |
| 0.0000001 | mn_log_loss | binary | 0.625611727821107 | 5 | 0.011736503208404 | pre0_mod01_post0 |
| 0.000000204335971785694 | mn_log_loss | binary | 0.625611727821107 | 5 | 0.011736503208404 | pre0_mod02_post0 |
| 0.00000041753189365604 | mn_log_loss | binary | 0.625611727821107 | 5 | 0.011736503208404 | pre0_mod03_post0 |
| 0.000000853167852417281 | mn_log_loss | binary | 0.625611727821107 | 5 | 0.011736503208404 | pre0_mod04_post0 |
| 0.00000174332882219999 | mn_log_loss | binary | 0.625611727821107 | 5 | 0.011736503208404 | pre0_mod05_post0 |

Penalty search history:

| iteration | log10_lower | log10_upper | penalty_lower | penalty_upper | best_penalty | best_cv_mn_log_loss | boundary_hit |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | -7 | 2 | 0.0000001 | 100 | 0.0788046281566992 | 0.619050049619958 | none |

Coefficients:

| term | estimate |
| --- | --- |
| (Intercept) | 0.354717734852242 |
| distance | -0.270169167546421 |
| angle | -0.236958956577396 |
| xCoordNorm | 0.203265059127165 |
| zoneCode_O | 0.197801321822511 |
| dDistance | -0.127455908331099 |
| shotType_wrist | 0.124971900564186 |
| dDistancePerSecond | -0.124726106703935 |
| typeDescKeyPrev_takeaway.against | -0.118103256286942 |
| dAngle | -0.110436534960248 |
| typeDescKeyPrev_blocked.shot.for | -0.108754384690712 |
| shotType_other | 0.0971747656393807 |
| shotType_snap | 0.0962738518350921 |
| typeDescKeyPrev_blocked.shot.against | 0.0925022630721288 |
| dXCoordNormPerSecond | 0.0918373126571759 |
| corsiFor | 0.0904945409974212 |
| dYCoordNormPerSecond | -0.0801403946104311 |
| dXCoordNorm | 0.0746545477413009 |
| typeDescKeyPrev_high.missed.shot.against | 0.0739856392187128 |
| isRush_yes | -0.0695702871478624 |
| typeDescKeyPrev_takeaway.for | 0.0672300751931744 |
| shotType_deflected | 0.0671821117988112 |
| minSecondsElapsedInShiftFor | 0.0641168301661183 |
| zoneCode_N | -0.0598907137956533 |
| typeDescKeyPrev_high.missed.shot.for | -0.0590053985408568 |
| isPlayoff_yes | 0.0575251720205186 |
| isHome_yes | -0.0566945137950058 |
| shotType_slap | 0.0551744905769729 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0.0528433721398789 |
| crossedRoyalRoad_yes | -0.052217754601809 |
| maxSecondsElapsedInShiftAgainst | 0.0510553602389469 |
| typeDescKeyPrev_post.missed.shot.for | 0.0483754211888114 |
| avgSecondsElapsedInShiftFor | 0.047693617302748 |
| maxSecondsElapsedInShiftFor | 0.0468297485136615 |
| shooterPositionCode_L | -0.0461318520855786 |
| shooterPositionCode_R | 0.0450225045867717 |
| secondsElapsedInPeriod | -0.0413868997295235 |
| corsiDifferential | 0.0388720614768496 |
| typeDescKeyPrev_giveaway.against | 0.0377602712471049 |
| shooterWeight | 0.0356828004296051 |
| secondsElapsedInGame | -0.033185999686545 |
| typeDescKeyPrev_other.shot.on.goal.against | -0.0330315952890531 |
| shooterSecondsElapsedSinceLastShift | -0.032008650791619 |
| skaterCountAgainst | 0.0319378196042782 |
| isRebound_yes | -0.031582947847325 |
| typeDescKeyPrev_given.hit | -0.0298755962141604 |
| typeDescKeyPrev_wide.missed.shot.for | -0.0281731400759732 |
| isOvertime_yes | 0.0275455804026513 |
| periodNumber | 0.0275420164885939 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0.025585188060601 |
| dAnglePerSecond | 0.0249654831269981 |
| typeDescKeyPrev_won.faceoff | -0.0238026662347925 |
| dYCoordNorm | 0.0237092558969364 |
| avgSecondsElapsedInShiftAgainst | 0.0234136478242052 |
| typeDescKeyPrev_lost.faceoff | -0.0233057365517316 |
| shooterAge | 0.0230147516180617 |
| fenwickFor | -0.0220253711735741 |
| goalsAgainst | 0.0219648839647062 |
| goalDifferential | -0.0217938408200372 |
| corsiAgainst | 0.0215338754500433 |
| maxSecondsElapsedSinceLastShiftFor | 0.0211716406704761 |
| typeDescKeyPrev_post.missed.shot.against | 0.0209281191014263 |
| maxSecondsElapsedSinceLastShiftAgainst | 0.0207191657021701 |
| minSecondsElapsedInShiftAgainst | 0.0206185556257471 |
| shotType_tip.in | -0.0204178959629067 |
| typeDescKeyPrev_wide.missed.shot.against | -0.0200018973960271 |
| minSecondsElapsedSinceLastShiftFor | 0.0199830894087906 |
| typeDescKeyPrev_snap.shot.on.goal.against | -0.0185343744426448 |
| shooterHeight | -0.0183118103674245 |
| avgSecondsElapsedSinceLastShiftFor | -0.018012259231511 |
| fenwickDifferential | -0.017694415971448 |
| typeDescKeyPrev_wrist.shot.on.goal.against | -0.0159296964665365 |
| manDifferential | -0.0156051954192575 |
| shotsAgainst | -0.0152688952746015 |
| shooterSecondsElapsedInShift | -0.0111227259313852 |
| typeDescKeyPrev_wrist.shot.on.goal.for | -0.00888714525076828 |
| goalsFor | 0.00859396273891678 |
| shotDifferential | 0.00857908808112452 |
| typeDescKeyPrev_other.missed.shot.against | 0.00808623823627942 |
| fenwickAgainst | 0.00787587459396838 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0.00759960802829426 |
| strengthState_penalty.kill | -0.00665786592435345 |
| skaterCountFor | 0.00663820254581055 |
| yCoordNorm | 0.00478245101651696 |
| typeDescKeyPrev_taken.hit | -0.00407699311584838 |
| minSecondsElapsedSinceLastShiftAgainst | -0.00395921747467336 |
| isBehindNet_yes | -0.0024857899040912 |
| typeDescKeyPrev_backhand.shot.on.goal.for | -0.00245170412772936 |
| shooterHandCode_R | -0.00245047922491156 |
| shotsFor | -0.00243607333814383 |
| avgSecondsElapsedSinceLastShiftAgainst | 0.00231039392800999 |
| shooterPositionCode_D | 0.00213568679413315 |
| typeDescKeyPrev_giveaway.for | -0.000969621157250325 |
| secondsElapsedInSequence | 0.00077290454166902 |
| dSecondsElapsedInSequence | 0.000550086373861084 |
| strengthState_power.play | 0.000100702790723349 |

Numeric imputation medians:

| term | median |
| --- | --- |
| periodNumber | 3 |
| secondsElapsedInPeriod | 1121 |
| secondsElapsedInGame | 3521 |
| secondsElapsedInSequence | 40 |
| xCoordNorm | 10.5 |
| yCoordNorm | 2 |
| dXCoordNorm | -17 |
| dYCoordNorm | 2 |
| distance | 83.3216420258632 |
| angle | 15.8405410595283 |
| dDistance | 15.8752504990759 |
| dAngle | -3.34331933161085 |
| dSecondsElapsedInSequence | 9 |
| dXCoordNormPerSecond | -1.23356643356643 |
| dYCoordNormPerSecond | 0.142857142857143 |
| dDistancePerSecond | 1.18973103263786 |
| dAnglePerSecond | -0.266761120656703 |
| goalsFor | 3 |
| goalsAgainst | 2 |
| goalDifferential | 2 |
| shotsFor | 27 |
| shotsAgainst | 28 |
| shotDifferential | 0 |
| fenwickFor | 40 |
| fenwickAgainst | 41 |
| fenwickDifferential | -1 |
| corsiFor | 55 |
| corsiAgainst | 59 |
| corsiDifferential | -4 |
| shooterHeight | 73 |
| shooterWeight | 200 |
| shooterAge | 28 |
| shooterSecondsElapsedInShift | 39 |
| shooterSecondsElapsedSinceLastShift | 129 |
| minSecondsElapsedInShiftFor | 24 |
| maxSecondsElapsedInShiftFor | 62 |
| avgSecondsElapsedInShiftFor | 43 |
| minSecondsElapsedInShiftAgainst | 26 |
| maxSecondsElapsedInShiftAgainst | 83 |
| avgSecondsElapsedInShiftAgainst | 56 |
| minSecondsElapsedSinceLastShiftFor | 84 |
| maxSecondsElapsedSinceLastShiftFor | 182.5 |
| avgSecondsElapsedSinceLastShiftFor | 131 |
| minSecondsElapsedSinceLastShiftAgainst | 79 |
| maxSecondsElapsedSinceLastShiftAgainst | 207 |
| avgSecondsElapsedSinceLastShiftAgainst | 141.75 |
| skaterCountFor | 5 |
| skaterCountAgainst | 6 |
| manDifferential | 0 |
| isPlayoff_yes | 0 |
| isPlayoff_unknown | 0 |
| isPlayoff_new | 0 |
| isHome_yes | 1 |
| isHome_unknown | 0 |
| isHome_new | 0 |
| isOvertime_yes | 0 |
| isOvertime_unknown | 0 |
| isOvertime_new | 0 |
| zoneCode_N | 0 |
| zoneCode_O | 0 |
| zoneCode_unknown | 0 |
| zoneCode_new | 0 |
| isBehindNet_yes | 0 |
| isBehindNet_unknown | 0 |
| isBehindNet_new | 0 |
| crossedRoyalRoad_yes | 0 |
| crossedRoyalRoad_unknown | 0 |
| crossedRoyalRoad_new | 0 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0 |
| typeDescKeyPrev_blocked.shot.against | 0 |
| typeDescKeyPrev_blocked.shot.for | 0 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0 |
| typeDescKeyPrev_giveaway.against | 0 |
| typeDescKeyPrev_giveaway.for | 0 |
| typeDescKeyPrev_given.hit | 0 |
| typeDescKeyPrev_high.missed.shot.against | 0 |
| typeDescKeyPrev_high.missed.shot.for | 0 |
| typeDescKeyPrev_lost.faceoff | 0 |
| typeDescKeyPrev_other.missed.shot.against | 0 |
| typeDescKeyPrev_other.shot.on.goal.against | 0 |
| typeDescKeyPrev_post.missed.shot.against | 0 |
| typeDescKeyPrev_post.missed.shot.for | 0 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0 |
| typeDescKeyPrev_takeaway.against | 0 |
| typeDescKeyPrev_takeaway.for | 0 |
| typeDescKeyPrev_taken.hit | 0 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0 |
| typeDescKeyPrev_wide.missed.shot.against | 0 |
| typeDescKeyPrev_wide.missed.shot.for | 0 |
| typeDescKeyPrev_won.faceoff | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0 |
| typeDescKeyPrev_unknown | 0 |
| typeDescKeyPrev_new | 0 |
| shotType_deflected | 0 |
| shotType_other | 0 |
| shotType_slap | 0 |
| shotType_snap | 0 |
| shotType_tip.in | 0 |
| shotType_wrist | 1 |
| shotType_unknown | 0 |
| shotType_new | 0 |
| isRebound_yes | 0 |
| isRebound_unknown | 0 |
| isRebound_new | 0 |
| isRush_yes | 0 |
| isRush_unknown | 0 |
| isRush_new | 0 |
| shooterHandCode_R | 0 |
| shooterHandCode_unknown | 0 |
| shooterHandCode_new | 0 |
| shooterPositionCode_D | 0 |
| shooterPositionCode_L | 0 |
| shooterPositionCode_R | 0 |
| shooterPositionCode_unknown | 0 |
| shooterPositionCode_new | 0 |
| isEmptyNetFor_yes | 0 |
| isEmptyNetFor_unknown | 0 |
| isEmptyNetFor_new | 0 |
| strengthState_penalty.kill | 0 |
| strengthState_power.play | 0 |
| strengthState_unknown | 0 |
| strengthState_new | 0 |

Numeric normalization parameters:

| term | mean | sd |
| --- | --- | --- |
| periodNumber | 3.00054704595186 | 0.0233890134862497 |
| secondsElapsedInPeriod | 1103.9556892779 | 85.6562017556009 |
| secondsElapsedInGame | 3504.61214442013 | 83.369089243355 |
| secondsElapsedInSequence | 59.1706783369803 | 61.3279016494276 |
| xCoordNorm | 5.35612691466083 | 55.2777886159891 |
| yCoordNorm | 2.2554704595186 | 24.7093193485481 |
| dXCoordNorm | -17.6542669584245 | 80.3179897205595 |
| dYCoordNorm | 1.94474835886214 | 34.0992259195283 |
| distance | 88.2727003505859 | 53.6196477394615 |
| angle | 20.1339572408435 | 17.4739164235797 |
| dDistance | 15.9174060727069 | 76.7843874091313 |
| dAngle | -9.09286163538376 | 33.4078113307549 |
| dSecondsElapsedInSequence | 14.4907002188184 | 15.2820381722192 |
| dXCoordNormPerSecond | -4.12189468669847 | 26.7157163853133 |
| dYCoordNormPerSecond | 0.424222607381299 | 10.8264559214393 |
| dDistancePerSecond | 3.71138985159924 | 24.738750434975 |
| dAnglePerSecond | -2.02327863414485 | 12.3883845004582 |
| goalsFor | 3.50601750547046 | 1.16905310872196 |
| goalsAgainst | 1.80579868708972 | 1.14184443833462 |
| goalDifferential | 1.70021881838074 | 0.68995447065572 |
| shotsFor | 27.8254923413567 | 6.33135964652686 |
| shotsAgainst | 27.7231947483589 | 6.65466639749042 |
| shotDifferential | 0.102297592997812 | 9.85427122545568 |
| fenwickFor | 40.664113785558 | 8.53979915471139 |
| fenwickAgainst | 41.6012035010941 | 8.94030183543913 |
| fenwickDifferential | -0.937089715536105 | 14.1915846792149 |
| corsiFor | 55.8654266958425 | 11.1746569694012 |
| corsiAgainst | 59.410284463895 | 11.8391030281085 |
| corsiDifferential | -3.54485776805252 | 19.304793521508 |
| shooterHeight | 73.1400437636761 | 2.08864168331482 |
| shooterWeight | 200.170131291028 | 15.5490658182651 |
| shooterAge | 28.335886214442 | 4.027258372255 |
| shooterSecondsElapsedInShift | 44.0032822757112 | 29.0161608736929 |
| shooterSecondsElapsedSinceLastShift | 141.63238512035 | 77.1485213207736 |
| minSecondsElapsedInShiftFor | 28.6487964989059 | 21.9316314310226 |
| maxSecondsElapsedInShiftFor | 68.2778993435449 | 36.199568676676 |
| avgSecondsElapsedInShiftFor | 46.1196754194019 | 23.7412133578949 |
| minSecondsElapsedInShiftAgainst | 34.2861050328228 | 29.4254354101901 |
| maxSecondsElapsedInShiftAgainst | 90.187636761488 | 48.2522372753314 |
| avgSecondsElapsedInShiftAgainst | 62.370705689278 | 34.8239560877368 |
| minSecondsElapsedSinceLastShiftFor | 89.4507658643326 | 37.7959590496911 |
| maxSecondsElapsedSinceLastShiftFor | 195.683260393873 | 84.8320779495799 |
| avgSecondsElapsedSinceLastShiftFor | 136.500045587163 | 45.2629883216787 |
| minSecondsElapsedSinceLastShiftAgainst | 86.4365426695842 | 45.0606332310862 |
| maxSecondsElapsedSinceLastShiftAgainst | 220.989059080963 | 102.503382888845 |
| avgSecondsElapsedSinceLastShiftAgainst | 146.3280269876 | 53.3312720488343 |
| skaterCountFor | 4.92341356673961 | 0.268056619087125 |
| skaterCountAgainst | 5.96280087527352 | 0.197785301429958 |
| manDifferential | -0.0393873085339169 | 0.289540444991748 |
| isPlayoff_yes | 0.0716630196936543 | 0.257999699193369 |
| isHome_yes | 0.549781181619256 | 0.497651799843628 |
| isOvertime_yes | 0.000547045951859956 | 0.0233890134862497 |
| zoneCode_N | 0.219365426695842 | 0.413929904702594 |
| zoneCode_O | 0.433807439824945 | 0.495734791145492 |
| isBehindNet_yes | 0.00218818380743982 | 0.0467396055886259 |
| crossedRoyalRoad_yes | 0.467177242888403 | 0.499058025970799 |
| typeDescKeyPrev_backhand.shot.on.goal.for | 0.00109409190371991 | 0.033068006555834 |
| typeDescKeyPrev_blocked.shot.against | 0.158643326039387 | 0.365443126090017 |
| typeDescKeyPrev_blocked.shot.for | 0.0486870897155361 | 0.215271940156083 |
| typeDescKeyPrev_deflected.shot.on.goal.against | 0.000547045951859956 | 0.0233890134862498 |
| typeDescKeyPrev_giveaway.against | 0.102844638949672 | 0.303838972884656 |
| typeDescKeyPrev_giveaway.for | 0.024617067833698 | 0.154997452081897 |
| typeDescKeyPrev_given.hit | 0.0361050328227571 | 0.186602539761614 |
| typeDescKeyPrev_high.missed.shot.against | 0.00437636761487965 | 0.0660272665106512 |
| typeDescKeyPrev_high.missed.shot.for | 0.000547045951859956 | 0.0233890134862499 |
| typeDescKeyPrev_lost.faceoff | 0.0968271334792122 | 0.295803154060147 |
| typeDescKeyPrev_other.missed.shot.against | 0.00929978118161925 | 0.0960121768638606 |
| typeDescKeyPrev_other.shot.on.goal.against | 0.000547045951859956 | 0.0233890134862496 |
| typeDescKeyPrev_post.missed.shot.against | 0.00547045951859956 | 0.073780156154304 |
| typeDescKeyPrev_post.missed.shot.for | 0.00984682713347921 | 0.0987684346011507 |
| typeDescKeyPrev_slap.shot.on.goal.against | 0.0175054704595186 | 0.131180954305362 |
| typeDescKeyPrev_snap.shot.on.goal.against | 0.0164113785557987 | 0.127086114440107 |
| typeDescKeyPrev_takeaway.against | 0.0164113785557987 | 0.127086114440107 |
| typeDescKeyPrev_takeaway.for | 0.0618161925601751 | 0.240887305945499 |
| typeDescKeyPrev_taken.hit | 0.0563457330415755 | 0.230651239431042 |
| typeDescKeyPrev_tip.in.shot.on.goal.against | 0.00547045951859956 | 0.0737801561543048 |
| typeDescKeyPrev_wide.missed.shot.against | 0.0656455142231947 | 0.24772919238633 |
| typeDescKeyPrev_wide.missed.shot.for | 0.0421225382932166 | 0.200923653356351 |
| typeDescKeyPrev_won.faceoff | 0.148796498905908 | 0.355985147794465 |
| typeDescKeyPrev_wrist.shot.on.goal.against | 0.062363238512035 | 0.241880281344619 |
| typeDescKeyPrev_wrist.shot.on.goal.for | 0.00328227571115973 | 0.0572126998052959 |
| shotType_deflected | 0.00164113785557986 | 0.0404887801390721 |
| shotType_other | 0.0136761487964989 | 0.116174416088586 |
| shotType_slap | 0.0169584245076586 | 0.129150923539273 |
| shotType_snap | 0.0962800875273523 | 0.295055684441571 |
| shotType_tip.in | 0.0087527352297593 | 0.0931712063492202 |
| shotType_wrist | 0.769146608315099 | 0.421494116131832 |
| isRebound_yes | 0.0202407002188184 | 0.140861168131654 |
| isRush_yes | 0.0820568927789934 | 0.274526478009249 |
| shooterHandCode_R | 0.341903719912473 | 0.474477314503542 |
| shooterPositionCode_D | 0.135667396061269 | 0.342528738043647 |
| shooterPositionCode_L | 0.25437636761488 | 0.435629252554216 |
| shooterPositionCode_R | 0.202407002188184 | 0.401903931579602 |
| strengthState_penalty.kill | 0.0612691466083151 | 0.239888972197281 |
| strengthState_power.play | 0.0207877461706783 | 0.142712148508711 |

Dummy map:

| variable | level | output_column |
| --- | --- | --- |
| isPlayoff | yes | isPlayoff_yes |
| isPlayoff | unknown | isPlayoff_unknown |
| isPlayoff | new | isPlayoff_new |
| isHome | yes | isHome_yes |
| isHome | unknown | isHome_unknown |
| isHome | new | isHome_new |
| isOvertime | yes | isOvertime_yes |
| isOvertime | unknown | isOvertime_unknown |
| isOvertime | new | isOvertime_new |
| zoneCode | N | zoneCode_N |
| zoneCode | O | zoneCode_O |
| zoneCode | unknown | zoneCode_unknown |
| zoneCode | new | zoneCode_new |
| isBehindNet | yes | isBehindNet_yes |
| isBehindNet | unknown | isBehindNet_unknown |
| isBehindNet | new | isBehindNet_new |
| crossedRoyalRoad | yes | crossedRoyalRoad_yes |
| crossedRoyalRoad | unknown | crossedRoyalRoad_unknown |
| crossedRoyalRoad | new | crossedRoyalRoad_new |
| typeDescKeyPrev | backhand-shot-on-goal-for | typeDescKeyPrev_backhand.shot.on.goal.for |
| typeDescKeyPrev | blocked-shot-against | typeDescKeyPrev_blocked.shot.against |
| typeDescKeyPrev | blocked-shot-for | typeDescKeyPrev_blocked.shot.for |
| typeDescKeyPrev | deflected-shot-on-goal-against | typeDescKeyPrev_deflected.shot.on.goal.against |
| typeDescKeyPrev | giveaway-against | typeDescKeyPrev_giveaway.against |
| typeDescKeyPrev | giveaway-for | typeDescKeyPrev_giveaway.for |
| typeDescKeyPrev | given-hit | typeDescKeyPrev_given.hit |
| typeDescKeyPrev | high-missed-shot-against | typeDescKeyPrev_high.missed.shot.against |
| typeDescKeyPrev | high-missed-shot-for | typeDescKeyPrev_high.missed.shot.for |
| typeDescKeyPrev | lost-faceoff | typeDescKeyPrev_lost.faceoff |
| typeDescKeyPrev | other-missed-shot-against | typeDescKeyPrev_other.missed.shot.against |
| typeDescKeyPrev | other-shot-on-goal-against | typeDescKeyPrev_other.shot.on.goal.against |
| typeDescKeyPrev | post-missed-shot-against | typeDescKeyPrev_post.missed.shot.against |
| typeDescKeyPrev | post-missed-shot-for | typeDescKeyPrev_post.missed.shot.for |
| typeDescKeyPrev | slap-shot-on-goal-against | typeDescKeyPrev_slap.shot.on.goal.against |
| typeDescKeyPrev | snap-shot-on-goal-against | typeDescKeyPrev_snap.shot.on.goal.against |
| typeDescKeyPrev | takeaway-against | typeDescKeyPrev_takeaway.against |
| typeDescKeyPrev | takeaway-for | typeDescKeyPrev_takeaway.for |
| typeDescKeyPrev | taken-hit | typeDescKeyPrev_taken.hit |
| typeDescKeyPrev | tip-in-shot-on-goal-against | typeDescKeyPrev_tip.in.shot.on.goal.against |
| typeDescKeyPrev | wide-missed-shot-against | typeDescKeyPrev_wide.missed.shot.against |
| typeDescKeyPrev | wide-missed-shot-for | typeDescKeyPrev_wide.missed.shot.for |
| typeDescKeyPrev | won-faceoff | typeDescKeyPrev_won.faceoff |
| typeDescKeyPrev | wrist-shot-on-goal-against | typeDescKeyPrev_wrist.shot.on.goal.against |
| typeDescKeyPrev | wrist-shot-on-goal-for | typeDescKeyPrev_wrist.shot.on.goal.for |
| typeDescKeyPrev | unknown | typeDescKeyPrev_unknown |
| typeDescKeyPrev | new | typeDescKeyPrev_new |
| shotType | deflected | shotType_deflected |
| shotType | other | shotType_other |
| shotType | slap | shotType_slap |
| shotType | snap | shotType_snap |
| shotType | tip-in | shotType_tip.in |
| shotType | wrist | shotType_wrist |
| shotType | unknown | shotType_unknown |
| shotType | new | shotType_new |
| isRebound | yes | isRebound_yes |
| isRebound | unknown | isRebound_unknown |
| isRebound | new | isRebound_new |
| isRush | yes | isRush_yes |
| isRush | unknown | isRush_unknown |
| isRush | new | isRush_new |
| shooterHandCode | R | shooterHandCode_R |
| shooterHandCode | unknown | shooterHandCode_unknown |
| shooterHandCode | new | shooterHandCode_new |
| shooterPositionCode | D | shooterPositionCode_D |
| shooterPositionCode | L | shooterPositionCode_L |
| shooterPositionCode | R | shooterPositionCode_R |
| shooterPositionCode | unknown | shooterPositionCode_unknown |
| shooterPositionCode | new | shooterPositionCode_new |
| isEmptyNetFor | yes | isEmptyNetFor_yes |
| isEmptyNetFor | unknown | isEmptyNetFor_unknown |
| isEmptyNetFor | new | isEmptyNetFor_new |
| strengthState | penalty-kill | strengthState_penalty.kill |
| strengthState | power-play | strengthState_power.play |
| strengthState | unknown | strengthState_unknown |
| strengthState | new | strengthState_new |

Unknown categorical fills:

| variable | value |
| --- | --- |
| isPlayoff | unknown |
| isHome | unknown |
| isOvertime | unknown |
| zoneCode | unknown |
| isBehindNet | unknown |
| crossedRoyalRoad | unknown |
| typeDescKeyPrev | unknown |
| shotType | unknown |
| isRebound | unknown |
| isRush | unknown |
| shooterHandCode | unknown |
| shooterPositionCode | unknown |
| isEmptyNetFor | unknown |
| strengthState | unknown |

Novel categorical buckets:

| variable | value |
| --- | --- |
| isPlayoff | new |
| isHome | new |
| isOvertime | new |
| zoneCode | new |
| isBehindNet | new |
| crossedRoyalRoad | new |
| typeDescKeyPrev | new |
| shotType | new |
| isRebound | new |
| isRush | new |
| shooterHandCode | new |
| shooterPositionCode | new |
| isEmptyNetFor | new |
| strengthState | new |

Zero-variance terms removed before fitting:

| term |
| --- |
| isPlayoff_unknown |
| isPlayoff_new |
| isHome_unknown |
| isHome_new |
| isOvertime_unknown |
| isOvertime_new |
| zoneCode_unknown |
| zoneCode_new |
| isBehindNet_unknown |
| isBehindNet_new |
| crossedRoyalRoad_unknown |
| crossedRoyalRoad_new |
| typeDescKeyPrev_unknown |
| typeDescKeyPrev_new |
| shotType_unknown |
| shotType_new |
| isRebound_unknown |
| isRebound_new |
| isRush_unknown |
| isRush_new |
| shooterHandCode_unknown |
| shooterHandCode_new |
| shooterPositionCode_unknown |
| shooterPositionCode_new |
| isEmptyNetFor_yes |
| isEmptyNetFor_unknown |
| isEmptyNetFor_new |
| strengthState_unknown |
| strengthState_new |

## SO: Shootout / Penalty Shot

- Raw predictor columns: `isPlayoff`, `isHome`, `xCoordNorm`, `yCoordNorm`, `distance`, `angle`, `shotType`, `goalsFor`, `goalsAgainst`, `goalDifferential`, `shotsFor`, `shotsAgainst`, `shotDifferential`, `fenwickFor`, `fenwickAgainst`, `fenwickDifferential`, `corsiFor`, `corsiAgainst`, `corsiDifferential`, `shooterHeight`, `shooterWeight`, `shooterHandCode`, `shooterPositionCode`, `shooterAge`, `goalieHeight`, `goalieWeight`, `goalieHandCode`, `goalieAge`
- Best penalty (`glmnet` lambda): `0.672335753649933`
- Final searched log10 range: `[-7, 2]`
- Search note: `none`

Training summary:

| seasons | games | rows | goal_rate |
| --- | --- | --- | --- |
| 2023,2024 | 230 | 1188 | 0.315656565656566 |

Best-penalty grouped CV metrics:

| .metric | mean | std_err |
| --- | --- | --- |
| mn_log_loss | 0.624135358640592 | 0.0120031762955264 |
| roc_auc | 0.526419103398221 | 0.0256522748468738 |
| pr_auc | 0.720480968075048 | 0.0185244583480261 |
| brier_class | 0.216268567879362 | 0.00569308015878577 |

Top grouped CV log-loss settings:

| penalty | .metric | .estimator | mean | n | std_err | .config |
| --- | --- | --- | --- | --- | --- | --- |
| 0.672335753649933 | mn_log_loss | binary | 0.624135358640592 | 5 | 0.0120031762955264 | pre0_mod23_post0 |
| 1.37382379588326 | mn_log_loss | binary | 0.624139158388827 | 5 | 0.0121895111868145 | pre0_mod24_post0 |
| 2.80721620394118 | mn_log_loss | binary | 0.624328402998328 | 5 | 0.0123547077922261 | pre0_mod25_post0 |
| 5.73615251044868 | mn_log_loss | binary | 0.624514799425904 | 5 | 0.0124697527940048 | pre0_mod26_post0 |
| 11.7210229753348 | mn_log_loss | binary | 0.624642118551154 | 5 | 0.0125394528362161 | pre0_mod27_post0 |
| 0.329034456231267 | mn_log_loss | binary | 0.624651105730121 | 5 | 0.0118547819469371 | pre0_mod22_post0 |
| 23.9502661998749 | mn_log_loss | binary | 0.624716707748962 | 5 | 0.0125770144374655 | pre0_mod28_post0 |
| 48.939009184775 | mn_log_loss | binary | 0.624797277539692 | 5 | 0.0126155664673177 | pre0_mod29_post0 |
| 100 | mn_log_loss | binary | 0.624797277539692 | 5 | 0.0126155664673177 | pre0_mod30_post0 |
| 0.161026202756094 | mn_log_loss | binary | 0.625937116952394 | 5 | 0.011785775935851 | pre0_mod21_post0 |

Penalty search history:

| iteration | log10_lower | log10_upper | penalty_lower | penalty_upper | best_penalty | best_cv_mn_log_loss | boundary_hit |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | -7 | 2 | 0.0000001 | 100 | 0.672335753649933 | 0.624135358640592 | none |

Coefficients:

| term | estimate |
| --- | --- |
| (Intercept) | -0.77589620259463 |
| shotType_other | 0.0375251538657816 |
| goalieWeight | 0.0297921844161965 |
| yCoordNorm | 0.0279002982206543 |
| shotType_snap | 0.025464751536339 |
| shooterHandCode_R | -0.0236819035128646 |
| shooterPositionCode_D | -0.0215647520311729 |
| isPlayoff_yes | 0.019922909230127 |
| goalsAgainst | -0.017749128677122 |
| goalsFor | -0.0160947459354104 |
| distance | -0.0152166119217884 |
| fenwickDifferential | 0.0151937469011123 |
| fenwickAgainst | -0.0151849237607375 |
| isHome_yes | 0.0142819383090187 |
| angle | -0.0130571450228975 |
| shotType_deflected | -0.0109369254092798 |
| shooterPositionCode_L | 0.0103486197145824 |
| shooterHeight | -0.00963499507040147 |
| goalieHandCode_R | 0.00854219620356224 |
| shooterWeight | -0.00824039695323443 |
| corsiAgainst | -0.00802551644681199 |
| xCoordNorm | 0.0077652910768733 |
| shotType_slap | -0.00723686520241828 |
| corsiDifferential | 0.00709525432714523 |
| goalieHeight | 0.00589497384444084 |
| shooterAge | -0.00573931780378883 |
| fenwickFor | 0.00494444944428132 |
| goalDifferential | 0.00486630492696181 |
| goalieAge | 0.0048352358975378 |
| shotsAgainst | -0.00353901859693891 |
| shotsFor | -0.00243014130617493 |
| corsiFor | 0.00154263765762475 |
| shotType_wrist | -0.00129046629650905 |
| shotDifferential | 0.000898503139099131 |
| shooterPositionCode_R | -0.000483630789042935 |

Numeric imputation medians:

| term | median |
| --- | --- |
| xCoordNorm | 76 |
| yCoordNorm | 0 |
| distance | 13.0384048104053 |
| angle | 10.7019765718589 |
| goalsFor | 3 |
| goalsAgainst | 3 |
| goalDifferential | 0 |
| shotsFor | 31 |
| shotsAgainst | 32 |
| shotDifferential | 0 |
| fenwickFor | 46 |
| fenwickAgainst | 46 |
| fenwickDifferential | 0 |
| corsiFor | 64 |
| corsiAgainst | 64 |
| corsiDifferential | 0 |
| shooterHeight | 73 |
| shooterWeight | 198 |
| shooterAge | 27 |
| goalieHeight | 75 |
| goalieWeight | 201 |
| goalieAge | 28 |
| isPlayoff_yes | 0 |
| isPlayoff_unknown | 0 |
| isPlayoff_new | 0 |
| isHome_yes | 1 |
| isHome_unknown | 0 |
| isHome_new | 0 |
| shotType_deflected | 0 |
| shotType_other | 0 |
| shotType_slap | 0 |
| shotType_snap | 0 |
| shotType_wrist | 1 |
| shotType_unknown | 0 |
| shotType_new | 0 |
| shooterHandCode_R | 0 |
| shooterHandCode_unknown | 0 |
| shooterHandCode_new | 0 |
| shooterPositionCode_D | 0 |
| shooterPositionCode_L | 0 |
| shooterPositionCode_R | 0 |
| shooterPositionCode_unknown | 0 |
| shooterPositionCode_new | 0 |
| goalieHandCode_R | 0 |
| goalieHandCode_unknown | 0 |
| goalieHandCode_new | 0 |

Numeric normalization parameters:

| term | mean | sd |
| --- | --- | --- |
| xCoordNorm | 75.699494949495 | 6.22870219226882 |
| yCoordNorm | -0.0193602693602694 | 4.24150419124922 |
| distance | 14.1045408156194 | 5.89357816774007 |
| angle | 15.8709338152364 | 16.2345267348797 |
| goalsFor | 3.05976430976431 | 1.44566188174572 |
| goalsAgainst | 3.2037037037037 | 1.49661333597733 |
| goalDifferential | -0.143939393939394 | 0.677217636167738 |
| shotsFor | 31.5429292929293 | 8.00751598317333 |
| shotsAgainst | 31.8535353535354 | 8.19653877016881 |
| shotDifferential | -0.310606060606061 | 10.6041743764868 |
| fenwickFor | 46.1666666666667 | 11.0668870912502 |
| fenwickAgainst | 46.4612794612795 | 11.3642020174699 |
| fenwickDifferential | -0.294612794612795 | 14.9613623557456 |
| corsiFor | 63.6456228956229 | 14.1315222765497 |
| corsiAgainst | 63.8947811447811 | 14.4674007554671 |
| corsiDifferential | -0.249158249158249 | 19.4379805470928 |
| shooterHeight | 72.733164983165 | 2.11115169973527 |
| shooterWeight | 197.473905723906 | 15.0293784494722 |
| shooterAge | 27.3939393939394 | 4.44318589483616 |
| goalieHeight | 74.8863636363636 | 1.59919395527211 |
| goalieWeight | 200.958754208754 | 16.2057375654101 |
| goalieAge | 28.4873737373737 | 3.72572859794707 |
| isPlayoff_yes | 0.000841750841750842 | 0.0290129426592825 |
| isHome_yes | 0.517676767676768 | 0.499897873208522 |
| shotType_deflected | 0.000841750841750842 | 0.0290129426592824 |
| shotType_other | 0.00252525252525252 | 0.0502095377080166 |
| shotType_slap | 0.0042087542087542 | 0.0647655107349197 |
| shotType_snap | 0.140572390572391 | 0.34772628989298 |
| shotType_wrist | 0.655723905723906 | 0.475331726883662 |
| shooterHandCode_R | 0.417508417508417 | 0.493355876615034 |
| shooterPositionCode_D | 0.0555555555555556 | 0.229157890873812 |
| shooterPositionCode_L | 0.202020202020202 | 0.401676301664132 |
| shooterPositionCode_R | 0.275252525252525 | 0.446829535431736 |
| goalieHandCode_R | 0.0555555555555556 | 0.229157890873811 |

Dummy map:

| variable | level | output_column |
| --- | --- | --- |
| isPlayoff | yes | isPlayoff_yes |
| isPlayoff | unknown | isPlayoff_unknown |
| isPlayoff | new | isPlayoff_new |
| isHome | yes | isHome_yes |
| isHome | unknown | isHome_unknown |
| isHome | new | isHome_new |
| shotType | deflected | shotType_deflected |
| shotType | other | shotType_other |
| shotType | slap | shotType_slap |
| shotType | snap | shotType_snap |
| shotType | wrist | shotType_wrist |
| shotType | unknown | shotType_unknown |
| shotType | new | shotType_new |
| shooterHandCode | R | shooterHandCode_R |
| shooterHandCode | unknown | shooterHandCode_unknown |
| shooterHandCode | new | shooterHandCode_new |
| shooterPositionCode | D | shooterPositionCode_D |
| shooterPositionCode | L | shooterPositionCode_L |
| shooterPositionCode | R | shooterPositionCode_R |
| shooterPositionCode | unknown | shooterPositionCode_unknown |
| shooterPositionCode | new | shooterPositionCode_new |
| goalieHandCode | R | goalieHandCode_R |
| goalieHandCode | unknown | goalieHandCode_unknown |
| goalieHandCode | new | goalieHandCode_new |

Unknown categorical fills:

| variable | value |
| --- | --- |
| isPlayoff | unknown |
| isHome | unknown |
| shotType | unknown |
| shooterHandCode | unknown |
| shooterPositionCode | unknown |
| goalieHandCode | unknown |

Novel categorical buckets:

| variable | value |
| --- | --- |
| isPlayoff | new |
| isHome | new |
| shotType | new |
| shooterHandCode | new |
| shooterPositionCode | new |
| goalieHandCode | new |

Zero-variance terms removed before fitting:

| term |
| --- |
| isPlayoff_unknown |
| isPlayoff_new |
| isHome_unknown |
| isHome_new |
| shotType_unknown |
| shotType_new |
| shooterHandCode_unknown |
| shooterHandCode_new |
| shooterPositionCode_unknown |
| shooterPositionCode_new |
| goalieHandCode_unknown |
| goalieHandCode_new |
