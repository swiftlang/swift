# swift_build_support/which.py - shutil.which() for Python 2.7 -*- python -*-
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
# A naive reimplementation of shutil.which() for Python 2.7. This can be
# removed if shutil.which() is backported, or if the Swift build toolchain
# migrates completely to Python 3.3+.
#
# ----------------------------------------------------------------------------

from __future__ import absolute_import

from . import cache_util
from . import shell


@cache_util.cached
def which(cmd):
    """
    Return the path to an executable which would be run if
    the given cmd was called. If no cmd would be called, return None.

    Python 3.3+ provides this behavior via the shutil.which() function;
    see: https://docs.python.org/3.3/library/shutil.html#shutil.which

    We provide our own implementation because shutil.which() has not
    been backported to Python 2.7, which we support.
    """
    out = shell.capture(['which', cmd],
                        dry_run=False, echo=False, optional=True)
    if out is None:
        return None
    return out.rstrip()
