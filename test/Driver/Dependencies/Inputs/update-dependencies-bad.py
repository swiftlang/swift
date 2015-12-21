#!/usr/bin/env python

# Fails if the input file is named "bad.swift"; otherwise dispatches to
# update-dependencies.py.

from __future__ import print_function

import os
import sys

assert sys.argv[1] == '-frontend'

primaryFile = sys.argv[sys.argv.index('-primary-file') + 1]

if os.path.basename(primaryFile) == 'bad.swift':
    print("Handled", os.path.basename(primaryFile))
    exit(1)

dir = os.path.dirname(os.path.abspath(__file__))
execfile(os.path.join(dir, "update-dependencies.py"))
