# swift_build_support/multiroot_data_file.py - Unified build -----*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os


def path():
    """product_source_name() -> str

    The path to the Xcode workspace to use for a unified build of multiple
    SwiftPM projects.
    """
    return os.path.join(os.path.dirname(__file__), '..',
                        'SwiftPM-Unified-Build.xcworkspace')
