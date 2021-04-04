# Monitoring "track selected"

The main goal of this patch is to provide a status, indicating whether or not
the track that the Max for Live device sits on is selected or not.

Actually, it will report is as selected only if all of the following conditions
are satisfied:

1. The track that the device sits on is selected.
2. The device is not disabled.
3. The device is not being saved.

In order to track all of this, it must monitor for changes to any of these
parameters, as well as the ID of the device itself. I've described the basic
approach in a blog post
[Designing a custom Push2 instrument in Max for Live/JavaScript: Part 1](http://edsko.net/2020/12/26/trichords-part1/), although the details of
the implementation differ. Although all the monitoring _logic_ is
[done in PureScript](/ps/src/TrackSelected.purs), the actual monitoring itself
are done using `live.observer` and `live.path` objects:

![](/docs/img/trackselected.png)

For the rationale for the delay after the `live.thisdevice`, see
https://cycling74.com/forums/livemax-hang-live-at-100-cpu-when-saving-patch.
