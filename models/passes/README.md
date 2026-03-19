# Pass Detection

This document explains exactly how `models/passes/detect.R` detects completed passes from the replay tracking data in `models/passes/data/`.

## Scope

The detector applies the same method season by season to every replay parquet in `models/passes/data/`. Each replay is a frame-by-frame tracking sequence leading up to a goal. For each season, the script:

1. finds the replay window for each non-penalty-shot, non-shootout goal,
2. detects completed passes from puck/player tracking within that replay,
3. inserts two synthetic play-by-play rows for every detected completed pass:
   - `pass`
   - `reception`

The detector is not using labeled pass events from NHL data (which is obviously not available). It is a heuristic built from player-to-puck proximity and possession changes.

## Inputs Used

The pass detector uses:

- `pbps <- nhlscraper::gc_pbps(20232024)`
- `replays <- arrow::read_parquet("models/passes/data/NHL_REPLAYS_20232024.parquet")`

From `pbps`, it uses:

- the goal event IDs,
- game context,
- the scoring window end time,
- home/away team identity,
- home team defending side in period 1,
- the most recent real PBP row before a synthetic pass/reception for context.

From `replays`, it uses:

- puck coordinates by frame,
- tracked player coordinates by frame,
- tracked player IDs and team IDs by frame.

Goalies are intentionally ignored during pass detection. Their tracked positions can still appear in the replay data and are still used when filling on-ice columns for synthetic rows, but goalie touches are not allowed to create or receive detected passes. It was simply too difficult to work with, for now.

The script writes one season-specific output file per replay parquet:

- `models/passes/data/pbps_trimmed_20232024.csv`
- `models/passes/data/pbps_trimmed_20242025.csv`
- `models/passes/data/pbps_trimmed_20252026.csv`

## Goal Windows

The script first finds replay-backed goals and excludes goals whose `situationCode` is:

- `1010`
- `0101`

Those are penalty-shot or shootout states and are intentionally removed from the pass-detection set.

For each remaining replay-goal pair, the replay window is:

- `goalSecondsElapsedInGame - replay_span_seconds`
- through
- `goalSecondsElapsedInGame`

where:

- `replay_span_seconds = (n_frames - 1) / 10`

That treats the replay timestamps as deciseconds.

Every retained row in `pbps_trimmed` gets an `eventIdGoal` column so the sequence stays tied to the goal it leads to.

## Coordinate System

The replay parquet already contains the same unnormalized rink coordinates used in `gc_pbp()`:

- `xCoord = (xCoordRaw - 1200) / 12`
- `yCoord = (510 - yCoordRaw) / 12`

Normalized coordinates are computed the same way `nhlscraper::.normalize_coordinates()` does it:

1. determine whether the event owner is home or away,
2. determine whether the home team is attacking positive x in the current period,
3. if the event owner is attacking toward negative x, flip both axes:
   - `xCoordNorm = -xCoord`
   - `yCoordNorm = -yCoord`

This is important: normalization flips both `x` and `y`, not just `x`.

## Possession Heuristic

Pass detection starts by asking a narrower question than "was there a pass?":

`At this exact frame, does the puck look like it belongs to one specific player?`

That is all the control heuristic is doing. It is a frame-level possession guess. The pass detector is built on top of those frame-level guesses later.

### Why the Detector Uses Two Nearest Players

Using only the nearest player is too weak. A player can be the nearest skater to the puck without actually controlling it:

- the puck may be loose in a board battle,
- two players may be reaching for it at the same time,
- tracking noise may put the puck slightly closer to the wrong player for one frame.

So the detector compares the nearest player with the second-nearest player. That comparison is a simple way to measure how ambiguous the frame is.

If the nearest player is clearly closer than everyone else, the frame looks like possession. If the two closest players are almost tied, the frame looks contested or loose.

At the moment, that comparison uses the second-nearest tracked player overall, not specifically the nearest opponent. So a nearby teammate can still make the frame ambiguous.

I tested an opponent-aware alternative on 2023-24. About `46%` of the currently ambiguous frames had a teammate, not an opponent, as the runner-up, and an opponent-aware rule would recover `11,203` frames that are currently treated as uncontrolled. I did **not** switch the production detector to that rule yet, because this project is trying to infer the exact player touch, not just team possession. Ignoring nearby teammates too early would make it easier to call a team-controlled frame, but it could also make the player-level credit noisier.

So the current detector is intentionally conservative:

- it keeps teammate proximity in the ambiguity check,
- it treats nearest-opponent logic as a plausible next refinement,
- but it does not yet use that refinement for production pass insertion.

### What Happens in One Frame

For each frame, the script does this:

1. compute the Euclidean distance from the puck to every tracked player,
2. identify the nearest tracked player,
3. identify the second-nearest tracked player,
4. decide whether the nearest player is close enough, and clearly enough in front of the second-nearest player, to count as control.

This produces either:

- `controlled by player X on team Y`, or
- `uncontrolled`.

### The 5-Foot Gate

The first rule is a basic reachability filter:

- if the nearest player is more than `5` feet from the puck, the frame is automatically treated as uncontrolled.

That rule says: if nobody is even reasonably close to the puck, do not call it possession.

So `5` feet is not "definite control". It is only the outer boundary for *possible* control.

### The Two Ways a Frame Can Become Controlled

Once a frame passes the `5`-foot gate, the script asks whether control is convincing enough. It allows two main ways for that to happen.

#### 1. The puck is very tight to the nearest player

If the nearest-player distance is at most `3.5` feet, the frame is treated as controlled even if another player is also nearby.

This is the "tight touch" rule. The puck is close enough to the nearest player that the detector is willing to call that frame possession.

#### 2. The nearest player is meaningfully closer than the runner-up

If the nearest-player distance is not under `3.5`, the detector can still call the frame controlled when:

- nearest distance is at most `5`, and
- nearest distance is at least `1` foot smaller than the second-nearest distance.

This is the "clear winner" rule. It says the puck is not glued to the player, but one player is still distinctly closer than the next candidate.

### Why the 1-Foot Clearance Matters

The `1` foot gap is there to reject ambiguous frames.

Example:

- nearest player = `4.7` feet away
- second-nearest player = `5.1` feet away

The nearest player is technically closer, but only by `0.4` feet. That is too small a margin to confidently call possession, so the frame is treated as uncontrolled.

By contrast:

- nearest player = `4.2` feet away
- second-nearest player = `5.6` feet away

Now the nearest player is `1.4` feet closer, which is enough separation to call the frame controlled.

### No Valid Second-Nearest Player

Occasionally only one valid tracked player is close enough to compare, or the remaining players for that frame are missing. In that case, if the nearest player is within `5` feet, the frame is allowed to count as controlled even without a second-nearest comparison.

That rule exists so missing tracking values do not automatically erase otherwise obvious possession frames.

### Concrete Examples

These examples are all for one frame only:

- nearest = `2.4`, second-nearest = `4.8` -> controlled
  - the puck is already within the `3.5` foot tight-touch zone
- nearest = `4.1`, second-nearest = `5.5` -> controlled
  - not a tight touch, but the nearest player is `1.4` feet clearer than the next player
- nearest = `4.7`, second-nearest = `5.1` -> uncontrolled
  - within `5` feet, but the frame is too contested because the margin is only `0.4`
- nearest = `6.2`, second-nearest = `7.0` -> uncontrolled
  - nobody is close enough to the puck

### What This Does Not Mean Yet

A controlled frame does **not** mean the detector has found a pass.

It only means:

- at this frame, player A appears to control the puck.

The pass is detected later, when the script sees a stable control segment for player A give way to a later stable control segment for player B on the same team, with limited uncontrolled time in between and enough puck travel to look like real movement.

The exact constants in `detect.R` are:

- `CONTROL_MAX_DIST = 5`
- `CONTROL_SECURE_DIST = 3.5`
- `CONTROL_CLEARANCE_MIN = 1`

If a frame does not satisfy the control rule, it is treated as uncontrolled.

### Why Stop at Two Nearby Players

The detector only needs the nearest player and the nearest challenger because the decision is local:

- either one player is clearly the best explanation for the puck location,
- or the closest challenger makes that frame ambiguous.

If the second-nearest player is already too far away to matter, the third- or fourth-nearest players matter even less. So the real modeling question is not "should we check four nearby players instead of two?" It is "should the challenger be the second-nearest player overall, or the nearest opponent?" That is the refinement I tested directly.

## Control Segments

After every frame is labeled with a controlling player/team or with no control, the script compresses consecutive identical control states into segments.

Each segment stores:

- start frame,
- end frame,
- controlling team,
- controlling player,
- number of frames in the segment,
- minimum puck distance within the segment.

A controlled segment is considered stable when:

- it has at least `2` frames, or
- its minimum puck distance is at most `3.5` feet.

That allows very short but very tight touches to count as real possession while rejecting more obvious noise.

## Completed Pass Rule

A completed pass is declared when the detector finds:

1. one stable controlled segment for player A,
2. followed by one later stable controlled segment for player B,
3. where A and B are on the same team,
4. and A and B are different players,
5. with no intervening controlled segment by another player,
6. with no incompatible real play-by-play event in the pass/reception window after accounting for integer-second PBP timing,
7. and with at most `12` uncontrolled frames between them,
8. and the puck has traveled at least `4` feet between the pass frame and the reception frame.

The exact constants are:

- `MAX_LOOSE_FRAMES = 12`
- `MIN_PASS_DISTANCE = 4`

Because the replay runs at 10 Hz, `12` loose frames means the puck can be uncontrolled for up to about `1.2` seconds and still be treated as one completed pass.

## Reconciling Replay Time with PBP Time

Replay-derived pass and reception times are in deciseconds, but NHL play-by-play times are integer seconds. So an apparent ordering conflict is not automatically a real conflict.

Example:

- replay-derived reception time = `2536.6`
- PBP shot time = `2536`

That does **not** prove the shot happened before the reception. It only proves that both events happened somewhere inside the `2536`th second of game time.

Because of that, the detector separates PBP overlaps into two cases.

### 1. True Interior-Second Conflicts

If a real PBP event falls in a whole second that is strictly between the pass second and the reception second, the pass candidate is discarded.

Example:

- pass at `2494.9`
- reception at `2496.0`
- PBP event at `2495`

That event sits in a genuinely interior second, so the candidate conflicts with the observed event sequence and is rejected.

On 2023-24, this happened only `18` times out of `14,925` raw pass candidates.

### 2. Same-Second Ambiguity

If every overlapping PBP event lives only in the same integer-second bucket as the pass endpoint or the reception endpoint, the detector treats the case as ambiguous rather than automatically invalid.

For these ambiguous cases, the pass is kept only when every overlapping PBP event is compatible with the detected receiver:

- the event has a player ID and that player is the detected receiver, or
- the event has no player ID and is a neutral reset row:
  - `stoppage`
  - `faceoff`
  - `period-end`
  - `period-start`

If any same-second overlapping event belongs to somebody other than the detected receiver, the candidate pass is discarded.

This rule was chosen after testing the 2023-24 season:

- `735` raw candidates overlapped at least one PBP event,
- only `18` were true interior-second conflicts,
- `717` were same-second ambiguities.

So dropping all overlaps is too aggressive, but keeping all overlaps is also too permissive.

### Sorting Compatible Same-Second Events

When a same-second overlap is receiver-compatible and the candidate pass is kept, the detector changes only the internal row ordering, not the stored replay time:

- `secondsElapsedInGame` stays replay-derived,
- but the synthetic `reception` row is sorted just ahead of the compatible same-second PBP event.

That prevents visibly illogical sequences such as:

- `pass`
- `shot-on-goal`
- `reception`

when the issue is only second-level timestamp granularity. In a compatible case, the rows can instead be ordered as:

- `pass`
- `reception`
- `shot-on-goal`

which is consistent with the replay timing and with the coarse PBP clock.

## Pass and Reception Timing

Once a pass is detected:

- the `pass` row is placed at the end frame of the passer’s control segment,
- the `reception` row is placed at the start frame of the receiver’s control segment.

The replay timestamps are converted back to game time by measuring how far each frame is from the replay’s final frame:

- `event_time = goal_time - ((max_timeStamp - frame_timeStamp) / 10)`

That gives decisecond-resolution event times inside the goal window.

## Synthetic PBP Rows

Each detected pass creates two synthetic rows:

- `eventTypeDescKey = "pass"`
- `eventTypeDescKey = "reception"`

For those rows:

- `playerId` is the passer on `pass` rows,
- `playerId` is the receiver on `reception` rows,
- `eventOwnerTeamId` is the team making/completing the pass,
- `xCoord` and `yCoord` come from the puck at that frame,
- `xCoordNorm` and `yCoordNorm` use the same normalization rule as `nhlscraper`,
- `eventIdGoal` is the goal event the sequence leads to.

The script also fills the on-ice goalie/skater ID columns from the replay frame:

- home skaters,
- away skaters,
- goalie for/against,
- skater IDs for/against.

To populate the rest of the row, the script copies the latest real PBP row at or before the synthetic event time, then overwrites the fields that should reflect the pass/reception itself.

## Sorting and Sequence Structure

After original rows and synthetic rows are combined, they are sorted within each `(gameId, eventIdGoal)` sequence by:

1. exact replay-derived event time,
2. synthetic priority (`pass` before `reception` before original rows at the same timestamp),
3. existing row order as a tie-breaker.

`sortOrder` is then re-numbered within each `(gameId, eventIdGoal)` sequence.

That means `sortOrder` is sequence-specific, not global across the whole game.

## What This Heuristic Captures Well

It is best at finding:

- clear same-team puck movement from one controlled touch to another,
- standard completed passes where the puck is briefly loose,
- one-touch receptions if the receiver’s touch is tight enough to count as stable control.

## Main Limitations

This is still a heuristic, so some edge cases remain:

- very noisy tracking near the puck can create false control changes,
- scrambles near the crease may be overcounted or undercounted,
- deflections and short redirections can look like receptions,
- very long uncontrolled puck travel is intentionally rejected,
- if multiple tracked players are similarly close to the puck, possession may be ambiguous.
