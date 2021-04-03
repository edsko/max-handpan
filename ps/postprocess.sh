#!/bin/bash

## Post-process the output of psc

n=`basename $1`

cat purescript-maxforlive/max-module-prefix.js $1-purescript purescript-maxforlive/max-module-suffix.js > ../patchers/$n
