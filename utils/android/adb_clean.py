#!/usr/bin/env python
# adb_reboot.py - Reboots and cleans an Android device. -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from adb.commands import DEVICE_TEMP_DIR, reboot, rmdir


if __name__ == '__main__':
    reboot()
    rmdir(DEVICE_TEMP_DIR)
