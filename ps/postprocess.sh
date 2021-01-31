#!/bin/bash

## Post-process the output of psc

cat purescript-maxforlive/max-module-prefix.js $1-purescript purescript-maxforlive/max-module-suffix.js > patchers/$1
