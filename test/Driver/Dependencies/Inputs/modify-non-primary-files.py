#!/usr/bin/env python

# modify-non-primary-files.py simulates a build where the user is modifying the
# source files during compilation.

import os
import shutil
import sys

assert sys.argv[1] == '-frontend'

if '-primary-file' in sys.argv:
  primaryFileIndex = sys.argv.index('-primary-file') + 1
  primaryFile = sys.argv[primaryFileIndex]

  # Modify all files after the primary file.
  # Ideally this would modify every non-primary file, but that's harder to
  # infer without actually parsing the arguments.
  for file in sys.argv[primaryFileIndex+1:]:
    if file.startswith('-'):
      break
    os.utime(file, None)

else:
  primaryFile = None

outputFile = sys.argv[sys.argv.index('-o') + 1]

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
with open(outputFile, 'a'):
    os.utime(outputFile, None)

if primaryFile:
  print "Handled", os.path.basename(primaryFile)
else:
  print "Produced", os.path.basename(outputFile)
