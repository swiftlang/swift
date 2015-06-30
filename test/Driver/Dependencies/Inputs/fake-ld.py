#!/usr/bin/env python

# Emulates the linker.

import os
import sys

outputFile = sys.argv[sys.argv.index('-o') + 1]

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
with open(outputFile, 'a'):
    os.utime(outputFile, None)

print "Linked", os.path.basename(outputFile)
