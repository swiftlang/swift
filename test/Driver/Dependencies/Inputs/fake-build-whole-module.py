#!/usr/bin/env python

# Emulates the frontend of a -whole-module-optimization compilation.

import os
import shutil
import sys

assert sys.argv[1] == '-frontend'
assert '-primary-file' not in sys.argv

outputFile = sys.argv[sys.argv.index('-o') + 1]

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
with open(outputFile, 'a'):
    os.utime(outputFile, None)

print "Produced", os.path.basename(outputFile)
