#!/usr/bin/env python
# update-dependencies-bad.py - Fails on bad.swift -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# Fails if the input file is named "bad.swift" or "crash.swift"; otherwise
# dispatches to update-dependencies.py. "crash.swift" results in an
# exit-by-SIGKILL
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import shutil
import signal
import subprocess
import sys

assert sys.argv[2] == '-frontend'

primaryFile = sys.argv[sys.argv.index('-primary-file') + 1]

if (os.path.basename(primaryFile) == 'bad.swift' or
        os.path.basename(primaryFile) == 'crash.swift'):
    print("Handled", os.path.basename(primaryFile))

    # Replace the dependencies file with the input file.
    try:
        depsFile = sys.argv[sys.argv.index(
            '-emit-reference-dependencies-path') + 1]

        returncode = subprocess.call([sys.argv[1], "--from-yaml",
                                      "--input-filename=" + primaryFile,
                                      "--output-filename=" + depsFile])
        # If the input is not valid YAML, just copy it over verbatim;
        # we're testing a case where we produced a corrupted output file.
        if returncode != 0:
            shutil.copyfile(primaryFile, depsFile)
    except ValueError:
        pass

    if os.path.basename(primaryFile) == 'bad.swift':
        sys.exit(1)
    else:
        sys.stdout.flush()
        os.kill(os.getpid(), signal.SIGKILL)

execDir = os.path.dirname(os.path.abspath(__file__))
exec(open(os.path.join(execDir, "update-dependencies.py")).read())
