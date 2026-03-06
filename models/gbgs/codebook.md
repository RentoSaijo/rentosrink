# GBG Basic Codebook

This document describes the column meanings for the basic game-by-game (GBG) datasets built by:

- [skater_basic.R](/Users/rsai_91/Desktop/Work/rentosrink/models/gbgs/skater_basic.R)
- [goalie_basic.R](/Users/rsai_91/Desktop/Work/rentosrink/models/gbgs/goalie_basic.R)

Current output files:

- Skaters aggregate: `data/gbgs/basic/skaters_{seasonId}.csv`
- Skaters per-player: `data/gbgs/basic/{playerId}_{seasonId}.csv`
- Goalies aggregate: `data/gbgs/basic/goalies_{seasonId}.csv`
- Goalies per-player: `data/gbgs/basic/{playerId}_{seasonId}.csv`

## General Rules

- Each row is one player in one game.
- `gameTypeId` appears only in skater basic GBGs. `2` means regular season and `3` means playoffs.
- Regular-season shootout events are excluded from all statistics. Concretely, `gameTypeId == 2` and `period == 5` is removed before aggregation.
- Penalty shots are still counted.
- Strength suffixes:
  - `_ev`: even strength
  - `_pp`: power play
  - `_sh`: short-handed / penalty kill
- Shot-event hierarchy is inclusive, not mutually exclusive:
  - goals are a subset of shots on goal
  - shots on goal are a subset of fenwick
  - fenwick is a subset of corsi
- Rush/rebound state modifiers:
  - `_neither_`: shot event was neither rush nor rebound
  - `_rush_`: rush only
  - `_rebound_`: rebound only
  - `_both_`: both rush and rebound

## Skater Basic GBG

### Identifier Columns

- `playerId`: skater ID. Present only in the aggregate skater file.
- `gameId`: NHL game ID.
- `gameTypeId`: `2` regular season, `3` playoffs.
- `gameDate`: game date.

### Non-State Skater Metrics

These columns exist as `{metric}_{strength}`.

- `mP`: minutes played, from `skater_game_report(..., category = "timeonice")`, converted from seconds to minutes.
- `iFW`: individual faceoffs won.
- `oFW`: on-ice faceoffs won by the player’s team while the player was on the ice.
- `iFL`: individual faceoffs lost.
- `oFL`: on-ice faceoffs lost by the player’s team while the player was on the ice.
- `iHG`: individual hits given.
- `oHG`: on-ice hits given by the player’s team.
- `iHT`: individual hits taken.
- `oHT`: on-ice hits taken by the player’s team.
- `iTW`: individual takeaways.
- `oTW`: on-ice takeaways by the player’s team.
- `iGW`: individual giveaways.
- `oGW`: on-ice giveaways by the player’s team.
- `iMD`: individual penalty minutes drawn.
- `oMD`: on-ice penalty minutes drawn by the player’s team.
- `iMC`: individual penalty minutes committed.
- `oMC`: on-ice penalty minutes committed by the player’s team.

### State-Modified Skater Metrics

These columns exist as `{metric}_{state}_{strength}`, where `state` is one of:

- `neither`
- `rush`
- `rebound`
- `both`

Shot-derived skater metrics:

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
- `iRCF`: individual rebounds created for.
- `oRCF`: on-ice rebounds created for.
- `oRCA`: on-ice rebounds created against.

### Skater Prefix Meaning

- `i`: individual. The player was directly credited with the event.
- `o`: on-ice. The player was on the ice for the event.

### Skater Direction Meaning

- `F`: for the player’s team.
- `A`: against the player’s team.
- `D`: drawn.
- `C`: committed.

## Goalie Basic GBG

### Identifier Columns

- `playerId`: goalie ID. Present only in the aggregate goalie file.
- `gameId`: NHL game ID.
- `gameDate`: game date.

### State-Modified Goalie Metrics

All goalie event columns use the form `{metric}_{state}_{strength}`.

States:

- `neither`
- `rush`
- `rebound`
- `both`

Goalie metrics:

- `cA`: corsi against.
- `fA`: fenwick against.
- `sA`: shots on goal against.
- `gA`: goals against.
- `apA`: primary assists against.
- `asA`: secondary assists against.
- `rgA`: rebounds given against.

### Goalie Event Notes

- `cA`, `fA`, `sA`, and `gA` follow the same inclusive shot hierarchy used for skaters.
- `apA` and `asA` are attributed from goal events against.
- `rgA` is based on `createdRebound == TRUE` against the goalie.
- For blocked shots, `goalieInNetId` is usually missing in raw play-by-play. The goalie build infers the defending goalie from `playerIdsAgainst` when exactly one goalie is present there.
- True empty-net events are not forced onto a goalie.

## Examples

- `iFW_ev`: skater’s faceoffs won at even strength in that game.
- `oCF_rush_pp`: on-ice corsi for on rush-only shot attempts during power-play time.
- `iGF_both_sh`: individual goals scored on a shot marked as both rush and rebound while short-handed.
- `gA_rebound_ev`: goalie’s goals against on rebound-only events at even strength.
- `apA_both_pp`: primary assists against on goals that were both rush and rebound during penalty-kill exposure from the goalie’s perspective.
