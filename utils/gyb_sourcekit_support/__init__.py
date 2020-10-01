# gyb_sourcekit_support/__init__.py - Helpers for building Sourcekit -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# This file needs to be here in order for Python to treat the
# utils/gyb_sourcekit_support/ directory as a module.
#
# ----------------------------------------------------------------------------
from .UIDs import UID_KEYS
from .UIDs import UID_KINDS
from .UIDs import UID_REQUESTS


def check_uid_duplication():
    all_external_names = [K.externalName for K in UID_KEYS] + \
        [R.externalName for R in UID_REQUESTS] +              \
        [K.externalName for K in UID_KINDS]
    return len(all_external_names) == len(set(all_external_names))
