# build_script/utils/printf.py ----------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
"""
Rough printf. Flush immediately.
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import
from __future__ import print_function

import sys

__all__ = [
    'printf',
    'printf_with_argv0',
]


def printf(message, *args, **kwargs):
    print(message.format(*args, **kwargs))
    # Flush everytime to prevent mixed output with sub commands.
    sys.stdout.flush()


def printf_with_argv0(message, *args, **kwargs):
    printf(sys.argv[0] + ": " + message, *args, **kwargs)
