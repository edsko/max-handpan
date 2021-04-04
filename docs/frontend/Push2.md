# Frontend: Ableton Push 2

![](/docs/img/FrontendPush2.png)

The frontend is split into two parts: setup code, and processing button presses.

## Setup

The configuration of the Push 2 is [done in PureScript](/ps/src/Frontend.purs)
(compiled to JavaScript, of course). This does two things:

1. Find the Push 2 controller, grab control over it's button matrix, configure
   the colors of the buttons, and set up a `live.observer` object to listen
   for button presses.

2. Set up look-up tables (LUTs) that are used when processing button presses;
   see below.

The innocuous "On" button at the top is a patcher in its own right,
[monitoring if the track of the Max for Live device is selected](/docs/frontend/TrackSelected.md).
When the user selects a different track, we release control over the Push
so that it can resume normal operations.

## Processing button presses

Once the configuration is done, no more JavaScript runs. In particular,
when playing the handpan no JavaScript needs to run _at all_; this is true
for the frontend but also for the rest of the Max Handpan infrastructure,
in order to minimize any danger of latency.

When a button is pressed, this is reported by the `live.observer`; the
position of the button ("column 3, row 5") and the velocity with which it was
pressed, then run through a few LUTs:

![](/docs/img/FrontendPush2-LUTs.png)

The first LUT maps buttons to MIDI notes; see the
[MIDI layout](/docs/frontend/MidiLayout.md) for the layout used.
The second LUT maps each button to a choice of
[velocity curve](/docs/frontend/VelocityCurve.md), a custom patcher in their
own right. Currently we have two such curves, one for ghost notes and one for
everything else.

In the main patch the velocity is then routed through the appropriate
curve, and the MIDI note and velocity are sent to `midiout`.
