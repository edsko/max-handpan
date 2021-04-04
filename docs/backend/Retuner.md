# Returner

The retuner has three inlets: inlets 2 and 3 are used for setup (and are cold
inlets), and inlet 1 are for actual note lookups. The patcher is quite simple:

![](/docs/img/retuner.png)

Any changes to configuration values are
[handled in PureScript](/src/ps/Retuner.purs), which sets up a LUT with
transposition values, as well as configure some UI elements (most importantly,
a `multislider`) to provide the user with visual feedback on quite how much
transposition is required to achieve the selected result; less transposition
of course means staying closer to the original audio sampler.

Incoming MIDI notes (actually, "retune indices", see the description of the
[Soniccouture backend](/docs/docs/backend/Soniccouture.md) for an example
use case) are just routed straight through the LUT and to the outlet
of the retuner.

Notes that are past the end of the scale are filtered out (using `split`).
