# swift_build_support/ninja.py - Detect host machine's Ninja -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from .which import which


def is_ninja_installed():
    """
    Return whether `ninja` or `ninja-build` are available on the host machine.
    """
    if which('ninja') or which('ninja-build'):
        return True
    else:
        return False
