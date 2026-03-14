# Skater Contract Feature Codebook

## Scope

This document describes the skater contract feature engineering implemented in [clean.R](/Users/rsai_91/Desktop/Work/rentosrink/models/contracts/skater/clean.R).

The cleaner now builds date-anchored predictor columns for:

- `models/contracts/data/skater_train.csv`
- `models/contracts/data/skater_validate.csv`
- `models/contracts/data/skater_test.csv`

The current output has:

- 22 contract/base columns
- 337 derived predictor columns
- 359 total columns

## Design Rules

- Naming uses camelCase.
- Expected-goal features always use lower-case `x` and upper-case `G`, for example `ixGF`, `oxGA`, and `xGPct`.
- Strength-specific columns always end with `_{state}` where the state is one of `_ev`, `_pp`, or `_sh`.
- Contract-level prefixes and window prefixes are concatenated directly in camelCase, for example:
  - `regLast20ToiPerGame`
  - `regLast20ixGF_evPer60`
  - `poLast10oxGA_shPer60`
  - `regTrend20v20ixGFShare_ev`

## Data Sources

### Contract rows

- [models/contracts/data/skater_contracts.csv](/Users/rsai_91/Desktop/Work/rentosrink/models/contracts/data/skater_contracts.csv)

### GBG sources

- Basic skater GBGs: `data/gbgs/basic/skaters_<season>.csv`
- Advanced skater GBGs: `data/gbgs/advanced/skaters_<season>.csv`
- Metric definitions: [scripts/gbgs/codebook.md](/Users/rsai_91/Desktop/Work/rentosrink/scripts/gbgs/codebook.md)

The cleaner joins basic and advanced GBGs on `playerId` and `gameId`.

## Row Selection Process

For each contract row:

1. Collect all skater GBG rows for that `playerId`.
2. Keep only games with `gameDate < dateOfSigning`.
3. Classify game type from `gameId`:
   - `202302...` = regular season
   - `202303...` = playoff
4. Build regular-season windows and playoff windows separately.
5. Build trend features from non-overlapping regular-season buckets.

Regular season and playoff games are intentionally not pooled into the same trailing window.

## Train / Validate / Test

- Train rows: all non-first contracts except `startSeasonId == 20262027`
- Validate rows: `startSeasonId == 20262027`
- Test rows: synthetic next-contract rows created from current last contracts
- Synthetic test rows use `dateOfSigning = 2026-03-01`

## Missing / Zero Handling

- Counts and TOI totals default to `0` when no games are available in a window.
- Per-60 rates default to `0` when the relevant TOI denominator is `0`.
- Share and percentage features default to `0` when the denominator is `0`.
- `daysSinceLastGame`, `daysSinceLastRegularGame`, and `daysSinceLastPlayoffGame` stay `NA` when no qualifying game exists.
- `lastGameWasPlayoff` is `NA` when the player has no prior game before signing.

This is deliberate. Exposure columns are included so the model can distinguish true zero production from empty windows.

The current boosting trainers in [train.R](/Users/rsai_91/Desktop/Work/rentosrink/models/contracts/skater/train.R) are designed to pass numeric `NA` values through to XGBoost and LightGBM directly rather than replacing them with sentinels or recipe-time median imputations.

## Prefixes

### History prefix

These are not windowed and are computed from all games before signing:

- `careerTotalGpPreSigning`
- `careerRegularGpPreSigning`
- `careerPlayoffGpPreSigning`
- `careerPlayoffGpSharePreSigning`
- `careerTotalToiPreSigning`
- `careerRegularToiPreSigning`
- `careerPlayoffToiPreSigning`
- `careerPlayoffToiSharePreSigning`
- `daysSinceLastGame`
- `daysSinceLastRegularGame`
- `daysSinceLastPlayoffGame`
- `lastGameWasPlayoff`

### Window prefixes

Regular-season windows:

- `regLast20`
- `regLast40`
- `regLast82`

Playoff windows:

- `poLast5`
- `poLast10`

Each window prefix expands into the same set of feature stems described below.

### Trend prefixes

Trend windows use regular-season games only.

- `regTrend20v20`: last 20 regular-season games minus the previous 20 regular-season games
- `regTrend40v42`: last 40 regular-season games minus games 41 to 82 before signing

Trend exposures:

- `regTrend20v20RecentGp`
- `regTrend20v20PriorGp`
- `regTrend40v42RecentGp`
- `regTrend40v42PriorGp`

## Window Feature Families

Each window prefix expands to the following feature stems.

### Exposure and usage

- `Gp`: games available in the window
- `WindowFillRatio`: `Gp / windowSize`
- `ToiTotal`: total TOI across all strengths
- `ToiPerGame`: `ToiTotal / Gp`
- `Toi_ev`
- `Toi_pp`
- `Toi_sh`
- `ToiShare_ev`: `Toi_ev / ToiTotal`
- `ToiShare_pp`: `Toi_pp / ToiTotal`
- `ToiShare_sh`: `Toi_sh / ToiTotal`

### All-strength style / discipline

- `iTwPer60`: takeaways per 60 across all strengths
- `iGwPer60`: giveaways per 60 across all strengths
- `iMdPer60`: penalties drawn per 60 across all strengths
- `iMcPer60`: penalties committed per 60 across all strengths
- `iHgPer60`: hits given per 60 across all strengths
- `iHtPer60`: hits taken per 60 across all strengths
- `FaceoffPct`: faceoff win percentage across all strengths

### Even-strength offense and on-ice impact

- `iCF_evPer60`
- `iSF_evPer60`
- `iGF_evPer60`
- `iAPF_evPer60`
- `iASF_evPer60`
- `ixGF_evPer60`
- `oCF_evPer60`
- `oCA_evPer60`
- `oSF_evPer60`
- `oSA_evPer60`
- `oGF_evPer60`
- `oGA_evPer60`
- `oxGF_evPer60`
- `oxGA_evPer60`

### Even-strength percentages and shares

- `CfPct_ev`: `oCF_ev / (oCF_ev + oCA_ev)`
- `SfPct_ev`: `oSF_ev / (oSF_ev + oSA_ev)`
- `GfPct_ev`: `oGF_ev / (oGF_ev + oGA_ev)`
- `xGPct_ev`: `oxGF_ev / (oxGF_ev + oxGA_ev)`
- `iCFShare_ev`: `iCF_ev / oCF_ev`
- `iSFShare_ev`: `iSF_ev / oSF_ev`
- `iGFShare_ev`: `iGF_ev / oGF_ev`
- `ixGFShare_ev`: `ixGF_ev / oxGF_ev`

### Even-strength finishing / results-over-process

- `FinishingAbovexG_evPer60`: `iGF_ev/60 - ixGF_ev/60`
- `OnIceGoalsAbovexG_evPer60`: `(oGF_ev/60 - oGA_ev/60) - (oxGF_ev/60 - oxGA_ev/60)`

### Power-play production

- `iCF_ppPer60`
- `iSF_ppPer60`
- `iGF_ppPer60`
- `iAPF_ppPer60`
- `iASF_ppPer60`
- `ixGF_ppPer60`
- `oGF_ppPer60`
- `oxGF_ppPer60`

### Short-handed defensive / disruption profile

- `oCA_shPer60`
- `oSA_shPer60`
- `oGA_shPer60`
- `oxGA_shPer60`
- `iTw_shPer60`
- `iGw_shPer60`
- `iMd_shPer60`
- `iMc_shPer60`

## Trend Feature Stems

Each trend prefix expands to the following metric stems:

- `ToiPerGame`
- `ToiShare_pp`
- `ToiShare_sh`
- `iCF_evPer60`
- `iSF_evPer60`
- `iGF_evPer60`
- `ixGF_evPer60`
- `oCF_evPer60`
- `oCA_evPer60`
- `oGF_evPer60`
- `oGA_evPer60`
- `oxGF_evPer60`
- `oxGA_evPer60`
- `CfPct_ev`
- `xGPct_ev`
- `iCFShare_ev`
- `ixGFShare_ev`
- `FinishingAbovexG_evPer60`

Interpretation:

- Positive value: the player improved in the recent bucket relative to the earlier bucket
- Negative value: the player declined in the recent bucket relative to the earlier bucket

## Feature Examples

- `regLast20ToiPerGame`: average TOI per game over the last 20 regular-season games before signing
- `regLast82ixGF_evPer60`: individual even-strength expected goals per 60 over the last 82 regular-season games before signing
- `poLast5xGPct_ev`: on-ice even-strength expected-goal share in the last 5 playoff games before signing
- `regTrend20v20iCF_evPer60`: recent-20 minus prior-20 change in individual even-strength corsi-for rate
- `regTrend40v42FinishingAbovexG_evPer60`: recent-40 minus prior-42 change in even-strength goals-above-expected finishing

## Current Feature Philosophy

This feature set is intentionally built around:

- actual signing-date cutoffs
- recent regular-season form as the primary signal
- separate playoff windows rather than mixed windows
- explicit exposure columns so sparse windows remain usable
- non-overlapping trend comparisons rather than overlapping rolling deltas

If the feature set is revised later, this file should be updated alongside [clean.R](/Users/rsai_91/Desktop/Work/rentosrink/models/contracts/skater/clean.R).
