#!/usr/bin/env python
# build_script/__main__.py --------------------------------------*- python -*-
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
The entry point for Swift build script
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import
from __future__ import print_function

import sys
import os.path

from . import env
from .utils import printf_with_argv0
from .exceptions import BuildError


def main():

    if not env.SWIFT_SOURCE_ROOT:
        printf_with_argv0(
            "Could not infer source root directory. "
            "Forgot to set $SWIFT_SOURCE_ROOT environment variable?")
        return 1

    if not os.path.isdir(env.SWIFT_SOURCE_ROOT):
        printf_with_argv0(
            "Source root directory '{0}' does not exists. "
            "Forgot to set $SWIFT_SOURCE_ROOT environment variable?",
            env.SWIFT_SOURCE_ROOT)
        return 1

    # Determine if we are invoked in the preset mode and dispatch accordingly.
    if any([(opt.startswith("--preset") or opt == "--show-presets")
            for opt in sys.argv[1:]]):
        from .main_preset import main as impl_main
    else:
        from .main_driver import main as impl_main

    try:
        return impl_main(sys.argv)
    except BuildError as e:
        printf_with_argv0("Error: {0}", e)
        return 1


if __name__ == "__main__":
    # Since this script is invoked from ./utils/build-script like:
    #
    #   python -m build_script ./utils/build-script --options ...
    #
    # argv[0] is "path/to/build_script/__main__.py".
    # Shift sys.argv so that we can process it normally.
    sys.argv = sys.argv[1:]
    sys.exit(main())
