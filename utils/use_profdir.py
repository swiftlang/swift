#!/usr/bin/env python

# utils/use_profdir.py
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# This script is used to help prevent profile data clobbering during code
# coverage profiling.

from __future__ import print_function

import socket
import sys
import subprocess
import os
import string
import random
from glob import glob
from profdata_merge_worker import SERVER_ADDRESS
import argparse

def random_string(N):
    """Return a random ascii_uppercase + digits string of length `N`"""
    return ''.join(random.choice(string.ascii_uppercase + string.digits)
                   for _ in range(N))

def main():
    # Grab passed in bash command
    cmd = sys.argv[1:]

    # Check if we're going to merge the profdata files together
    merged = True # False
    if cmd[0] == "--merged":
        merged = True
        cmd = cmd[1:]

    # Search arguments for test identifiers
    script_files = [f for f in cmd if f.endswith('.script')]
    gtests = [f.split('=')[1] for f in cmd if f.startswith('--gtest_filter')]

    # Generate directory name using first test identifier, defaulting to
    # random characters if no test identifier can be found
    if script_files:
        profdir = script_files[0] + '.profdir'
    elif gtests:
        profdir = os.path.join(os.path.dirname(cmd[0]), gtests[0] + '.profdir')
    else:
        profdir = random_string(12) + '.profdir'

    # Create the directory using the generated name
    try:
        os.makedirs(profdir)
    except OSError as e:
        ERROR_FILE_EXISTS = 17
        if e.errno != ERROR_FILE_EXISTS:
            raise

    os.environ["LLVM_PROFILE_FILE"] = os.path.join(profdir, "swift-%p.profraw")
    return_code = subprocess.call(cmd)

    # Send the filenames in this directory to the merge worker, if we're
    # merging the files
    if merged:
        profile_glob = os.path.join(profdir, '*.profraw')
        files = [os.path.join(profdir, fn) for fn in glob(profile_glob)]
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect(SERVER_ADDRESS)
        sock.send("\n".join(files))
        sock.close()

    return return_code

if __name__ == '__main__':
    exit(main())
