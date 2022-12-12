#!/bin/bash

cabal run parse-ableton-files -- examples/mk1-precision.adg create-luts --file-prefix sc_mk1_ 
cabal run parse-ableton-files -- examples/mk2-precision.adg create-luts --file-prefix sc_mk2_

cp out/*.{table,coll} ../ks-sampler
