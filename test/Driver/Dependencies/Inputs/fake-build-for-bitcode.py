#!/usr/bin/env python

# Emulates the frontend of an -embed-bitcode job. That means we have to handle
# -emit-bc and -c actions.

import os
import shutil
import sys

assert sys.argv[1] == '-frontend'

primaryFile = sys.argv[sys.argv.index('-primary-file') + 1]
outputFile = sys.argv[sys.argv.index('-o') + 1]

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
with open(outputFile, 'a'):
    os.utime(outputFile, None)

if '-emit-bc' in sys.argv:
  print "Handled", os.path.basename(primaryFile)
elif '-c' in sys.argv:
  print "Produced", os.path.basename(outputFile)
else:
  assert False, "unknown action"
