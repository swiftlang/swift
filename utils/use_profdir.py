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

import sys
import subprocess
import os
import string
import random

def random_string(N):
    """Return a random ascii_uppercase + digits string of length `N`"""
    return ''.join(random.choice(string.ascii_uppercase + string.digits)
                   for _ in range(N))

def main():
    # Grab passed in bash command
    cmd = sys.argv[1:]

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

    # cd into the new directory and execute the passed in command
    previous_cwd = os.getcwd()
    os.chdir(profdir)
    try:
        return_code = subprocess.call(cmd)
    finally:
        os.chdir(previous_cwd)

    return return_code

if __name__ == '__main__':
    exit(main())
