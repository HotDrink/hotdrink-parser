#!/bin/bash

gmake -s
gmake -s test
mkdir -p test/js
cd test/js
ln -s ../../hotdrink-parser.js 2>/dev/null
ln -s ../../hotdrink-parser-test.js 2>/dev/null
cat <<instructions
You will need to link hotdrink.js in the test/js directory to run the tests.
instructions

