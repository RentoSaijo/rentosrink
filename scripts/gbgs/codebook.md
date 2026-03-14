# GBG Codebook

This document describes the game-by-game (GBG) datasets built by:

- [skater_basic.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/gbgs/skater_basic.R)
- [goalie_basic.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/gbgs/goalie_basic.R)
- [team_basic.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/gbgs/team_basic.R)
- [skater_advanced.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/gbgs/skater_advanced.R)
- [goalie_advanced.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/gbgs/goalie_advanced.R)
- [team_advanced.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/gbgs/team_advanced.R)

## File Layout

### Basic

- Skaters aggregate: `data/gbgs/basic/skaters_{seasonId}.csv`
- Goalies aggregate: `data/gbgs/basic/goalies_{seasonId}.csv`
- Teams aggregate: `data/gbgs/basic/teams_{seasonId}.csv`

### Advanced

- Skaters aggregate: `data/gbgs/advanced/skaters_{seasonId}.csv`
- Goalies aggregate: `data/gbgs/advanced/goalies_{seasonId}.csv`
- Teams aggregate: `data/gbgs/advanced/teams_{seasonId}.csv`

GBGs are aggregate-only. There are no split per-entity GBG files.

## General Rules

- Each row is one entity in one game.
- `gameTypeId` is not written to the GBG outputs because it is inferable from `gameId`.
- Regular-season shootout events are excluded from all event counts. Concretely, `gameTypeId == 2` and `period == 5` is removed before aggregation.
- Penalty shots are still counted.
- Rows with missing or unrecognized `strengthState` / `situationCode` default to even strength before aggregation.
- Non-shootout penalty-shot situation codes `0101` and `1010` are forced into even strength before aggregation.
- Missed shots with `reason == "short"` are excluded from all shot-attempt-derived metrics because they are treated as non-attempts.
- Strength is taken from the situation-code-derived `strengthState` field, not inferred from the number of populated on-ice player slots.
- Strength suffixes:
  - `_ev`: even strength
  - `_pp`: power play
  - `_sh`: short-handed / penalty kill
- Shot-event hierarchy is inclusive, not mutually exclusive:
  - goals are a subset of shots on goal
  - shots on goal are a subset of fenwick
  - fenwick is a subset of corsi
- Rush and rebound metrics are separate count families, not mutually exclusive state partitions:
  - rush metrics are based on `isRush == TRUE`
  - rebound-chance metrics are based on `isRebound == TRUE`
  - rebound-created metrics are based on `createdRebound == TRUE`
- `createdRebound` is used as-is from play-by-play. It is not restricted to on-net attempts.

## Skater Basic GBG

### Identifier Columns

- `playerId`: skater ID. Present only in the aggregate skater file.
- `gameId`: NHL game ID.
- `teamId`: team ID for the skater in that game.
- `gameDate`: game date.

### Skater Metrics

All skater event columns use the form `{metric}_{strength}`.

Metrics:

- `mP`: minutes played, from `skater_game_report(..., category = "timeonice")`, converted to minutes.
- `iFW`: individual faceoffs won.
- `oFW`: on-ice faceoffs won by the skater's team while the skater was on the ice.
- `iFL`: individual faceoffs lost.
- `oFL`: on-ice faceoffs lost by the skater's team while the skater was on the ice.
- `iHG`: individual hits given.
- `oHG`: on-ice hits given by the skater's team.
- `iHT`: individual hits taken.
- `oHT`: on-ice hits taken by the skater's team.
- `iTW`: individual takeaways.
- `oTW`: on-ice takeaways by the skater's team.
- `iGW`: individual giveaways.
- `oGW`: on-ice giveaways by the skater's team.
- `iMD`: individual penalty minutes drawn.
- `iMC`: individual penalty minutes committed.
- `iCF`: individual corsi for.
- `oCF`: on-ice corsi for.
- `oCA`: on-ice corsi against.
- `iFF`: individual fenwick for.
- `oFF`: on-ice fenwick for.
- `oFA`: on-ice fenwick against.
- `iSF`: individual shots on goal for.
- `oSF`: on-ice shots on goal for.
- `oSA`: on-ice shots on goal against.
- `iGF`: individual goals for.
- `oGF`: on-ice goals for.
- `oGA`: on-ice goals against.
- `iAPF`: individual primary assists for.
- `oAPF`: on-ice primary assists for.
- `oAPA`: on-ice primary assists against.
- `iASF`: individual secondary assists for.
- `oASF`: on-ice secondary assists for.
- `oASA`: on-ice secondary assists against.
- `iRSF`: individual rush chances for.
- `oRSF`: on-ice rush chances for.
- `oRSA`: on-ice rush chances against.
- `iRBF`: individual rebound chances for.
- `oRBF`: on-ice rebound chances for.
- `oRBA`: on-ice rebound chances against.
- `iRCF`: individual rebounds created for.
- `oRCF`: on-ice rebounds created for.
- `oRCA`: on-ice rebounds created against.

### Skater Prefix Meaning

- `i`: individual. The skater was directly credited with the event.
- `o`: on-ice. The skater was on the ice for the event.

### Skater Direction Meaning

- `F`: for the skater's team.
- `A`: against the skater's team.
- `D`: drawn.
- `C`: committed.

## Goalie Basic GBG

### Identifier Columns

- `playerId`: goalie ID. Present only in the aggregate goalie file.
- `gameId`: NHL game ID.
- `teamId`: team ID for the goalie in that game.
- `gameDate`: game date.

### Goalie Metrics

All goalie event columns use the form `{metric}_{strength}`.

Metrics:

- `cA`: corsi against.
- `fA`: fenwick against.
- `sA`: shots on goal against.
- `gA`: goals against.
- `apA`: primary assists against.
- `asA`: secondary assists against.
- `mD`: penalty minutes drawn by the goalie.
- `mC`: penalty minutes committed by the goalie.
- `rsA`: rush chances against.
- `rbA`: rebound chances against.
- `rgA`: rebounds generated against.

### Goalie Notes

- `cA`, `fA`, `sA`, and `gA` follow the same inclusive shot hierarchy used for skaters.
- `apA` and `asA` are attributed from goal events against.
- `rsA` is based on `isRush == TRUE` against the goalie.
- `rbA` is based on `isRebound == TRUE` against the goalie.
- `rgA` is based on `createdRebound == TRUE` against the goalie.
- For blocked shots, the shift-enriched feed usually provides `goaliePlayerIdAgainst`, which is used directly as the defending goalie.
- True empty-net events are not forced onto a goalie.

## Team Basic GBG

### Identifier Columns

- `teamId`: team ID. Present only in the aggregate team file.
- `gameId`: NHL game ID.
- `gameDate`: game date.

### Team Metrics

All team event columns use the form `{metric}_{strength}`.

Metrics:

- `mP`: team minutes played at that strength in that game.
  - `mP_pp` comes from `team_game_report(..., category = "powerplaytime")` via `timeOnIcePp`.
  - `mP_sh` comes from `team_game_report(..., category = "penaltykilltime")` via `timeOnIceShorthanded`.
  - `mP_ev` is computed as actual game duration minus `mP_pp` minus `mP_sh`.
  - Actual game duration is taken from play-by-play as the maximum `secondsElapsedInGame`, so overtime is included and regular-season shootouts are excluded.
- `FW`: team faceoffs won.
- `FL`: team faceoffs lost.
- `HG`: team hits given.
- `HT`: team hits taken.
- `TW`: team takeaways.
- `GW`: team giveaways.
- `MD`: penalty minutes drawn by the team.
- `MC`: penalty minutes committed by the team.
- `CF`: team corsi for.
- `CA`: team corsi against.
- `FF`: team fenwick for.
- `FA`: team fenwick against.
- `SF`: team shots on goal for.
- `SA`: team shots on goal against.
- `GF`: team goals for.
- `GA`: team goals against.
- `APF`: team primary assists for.
- `APA`: team primary assists against.
- `ASF`: team secondary assists for.
- `ASA`: team secondary assists against.
- `RSF`: team rush chances for.
- `RSA`: team rush chances against.
- `RBF`: team rebound chances for.
- `RBA`: team rebound chances against.
- `RCF`: team rebounds created for.
- `RCA`: team rebounds created against.

### Team Notes

- Team GBGs intentionally exclude individual `i*` columns and keep only team-level counts.
- Team rows are built only for games that have play-by-play, so scheduled but unplayed placeholder games are excluded.
- Team metrics do not use an `o` prefix. They are already team-level by definition.

## Examples

- `iFW_ev`: skater faceoffs won at even strength in that game.
- `oCF_pp`: skater on-ice corsi for during power-play time.
- `iRSF_ev`: skater rush chances for at even strength.
- `oRBA_sh`: skater on-ice rebound chances against while short-handed.
- `rsA_ev`: goalie rush chances against at even strength.
- `rgA_pp`: goalie rebounds generated against on the penalty kill.
- `RSF_ev`: team rush chances for at even strength.
- `RCA_pp`: team rebounds created against on the penalty kill.

## Advanced GBG

### Advanced General Rules

- Advanced GBGs are game-by-game expected-goal summaries built from the SBS/xG pipeline.
- Strength suffixes use the same three-state convention as the basic GBGs:
  - `_ev`: even strength
  - `_pp`: power play
  - `_sh`: short-handed / penalty kill
- Advanced scripts use the season SBS files in `data/sbss` as the primary `xG` source.
- Skater advanced GBGs also join lightweight attempt context from [shared.R](/Users/rsai_91/Desktop/Work/rentosrink/scripts/sbss/shared.R), because the current SBS exports still do not retain on-ice skater lists, which are required for `oxGF` and `oxGA`.
- Regular-season shootouts are excluded before aggregation.
- Non-shootout penalty shots are stored as even strength.
- Rows with missing or blank `strengthState` default to even strength before aggregation.
- Advanced outputs are based on the corresponding basic GBG files as the row base, so entities still receive one row for every game in which they appeared, even if all advanced metrics are zero.

### Skater Advanced GBG

Identifier columns:

- `playerId`: skater ID. Present only in the aggregate skater file.
- `gameId`: NHL game ID.
- `teamId`: team ID for the skater in that game.

Metrics:

- `ixGF`: individual expected goals for. Sum of `xG` on shot attempts taken by the skater.
- `oxGF`: on-ice expected goals for. Sum of `xG` on shot attempts by the skater's team while the skater was on the ice.
- `oxGA`: on-ice expected goals against. Sum of `xG` on shot attempts by the opposition while the skater was on the ice.

All skater advanced event columns use the form `{metric}_{strength}`.

### Goalie Advanced GBG

Identifier columns:

- `playerId`: goalie ID. Present only in the aggregate goalie file.
- `gameId`: NHL game ID.
- `teamId`: team ID for the goalie in that game.

Metrics:

- `xGA`: expected goals against. Sum of `xG` on shot attempts faced by the goalie.

All goalie advanced event columns use the form `{metric}_{strength}`.

### Team Advanced GBG

Identifier columns:

- `teamId`: team ID. Present only in the aggregate team file.
- `gameId`: NHL game ID.

Metrics:

- `xGF`: team expected goals for. Sum of `xG` on the team's shot attempts.
- `xGA`: team expected goals against. Sum of `xG` on the opposition's shot attempts.

All team advanced event columns use the form `{metric}_{strength}`.
