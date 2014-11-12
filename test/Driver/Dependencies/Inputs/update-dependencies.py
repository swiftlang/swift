#!/usr/bin/env python

# update-dependencies.py simulates a Swift compilation for the purposes of
# dependency analysis. That means it has two tasks:
#
# 1. Update the main output of the compilation job.
# 2. Update the associated dependencies file, in case anything changed.
#
# Since we only care about the timestamp of the output, the script just makes
# sure the file's mtime is set to $NOW, creating it if it doesn't exist.
# We don't actually care about the content of the input file, either, so we
# actually stick *the new dependencies* in the input file, and copy that over
# the old dependencies (if present).

import sys
import os
import shutil

assert sys.argv[1] == '-frontend'

primaryFile = sys.argv[sys.argv.index('-primary-file') + 1]
depsFile = sys.argv[sys.argv.index('-emit-reference-dependencies-path') + 1]
outputFile = sys.argv[sys.argv.index('-o') + 1]

# Replace the dependencies file with the input file.
shutil.copyfile(primaryFile, depsFile)

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
with open(outputFile, 'a'):
    os.utime(outputFile, None)

print "Handled", os.path.basename(primaryFile)
