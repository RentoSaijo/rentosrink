# GBG Basic Codebook

This document describes the basic game-by-game (GBG) datasets built by:

- [skater_basic.R](/Users/rsai_91/Desktop/Work/rentosrink/models/gbgs/skater_basic.R)
- [goalie_basic.R](/Users/rsai_91/Desktop/Work/rentosrink/models/gbgs/goalie_basic.R)
- [team_basic.R](/Users/rsai_91/Desktop/Work/rentosrink/models/gbgs/team_basic.R)

## File Layout

- Skaters aggregate: `data/gbgs/basic/skaters_{seasonId}.csv`
- Skaters split: `data/gbgs/basic/skater/{playerId}_{seasonId}.csv`
- Goalies aggregate: `data/gbgs/basic/goalies_{seasonId}.csv`
- Goalies split: `data/gbgs/basic/goalie/{playerId}_{seasonId}.csv`
- Teams aggregate: `data/gbgs/basic/teams_{seasonId}.csv`
- Teams split: `data/gbgs/basic/team/{teamId}_{seasonId}.csv`

Aggregate files keep the entity ID column. Split files drop it because it is already encoded in the filename.

## General Rules

- Each row is one entity in one game.
- `gameTypeId` is not written to the GBG outputs because it is inferable from `gameId`.
- Regular-season shootout events are excluded from all event counts. Concretely, `gameTypeId == 2` and `period == 5` is removed before aggregation.
- Penalty shots are still counted.
- Missed shots with `reason == "short"` are excluded from all shot-attempt-derived metrics because they are treated as non-attempts.
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
- `oMD`: on-ice penalty minutes drawn by the skater's team.
- `iMC`: individual penalty minutes committed.
- `oMC`: on-ice penalty minutes committed by the skater's team.
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
- `rsA`: rush chances against.
- `rbA`: rebound chances against.
- `rgA`: rebounds generated against.

### Goalie Notes

- `cA`, `fA`, `sA`, and `gA` follow the same inclusive shot hierarchy used for skaters.
- `apA` and `asA` are attributed from goal events against.
- `rsA` is based on `isRush == TRUE` against the goalie.
- `rbA` is based on `isRebound == TRUE` against the goalie.
- `rgA` is based on `createdRebound == TRUE` against the goalie.
- For blocked shots, raw play-by-play usually has no `goalieInNetId`. The goalie build infers the defending goalie from `playerIdsAgainst` when a defending goalie can be identified there.
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
