#!/usr/bin/env python

# Like /bin/touch, but takes a time using the LLVM epoch.

import os
import sys

assert len(sys.argv) >= 2
timeVal = int(sys.argv[1])

timeVal += 946684800 # offset between Unix and LLVM epochs

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
for outputFile in sys.argv[1:]:
  with open(outputFile, 'a'):
    os.utime(outputFile, (timeVal, timeVal))
