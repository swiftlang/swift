#!/usr/bin/env python
# sourcekitd_path_sanitize.py - Cleans up paths from sourcekitd-test output
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import re
import sys

SWIFTMODULE_BUNDLE_RE = re.compile(
    r'key.filepath: ".*[/\\](.*)\.swiftmodule[/\\].*\.swiftmodule"')
SWIFTMODULE_RE = re.compile(r'key.filepath: ".*[/\\](.*)\.swiftmodule"')
SWIFT_RE = re.compile(r'key.filepath: ".*[/\\](.*)\.swift"')
PCM_RE = re.compile(r'key.filepath: ".*[/\\](.*)-[0-9A-Z]*\.pcm"')
HEADER_RE = re.compile(r' file=\\".*[/\\](.*)\.h\\"')

try:
    for line in sys.stdin.readlines():
        line = re.sub(SWIFTMODULE_BUNDLE_RE,
                      r'key.filepath: \1.swiftmodule', line)
        line = re.sub(SWIFTMODULE_RE, r'key.filepath: \1.swiftmodule', line)
        line = re.sub(SWIFT_RE, r'key.filepath: \1.swift', line)
        line = re.sub(PCM_RE, r'key.filepath: \1.pcm', line)
        line = re.sub(HEADER_RE, r' file=\1.h', line)
        sys.stdout.write(line)
except KeyboardInterrupt:
    sys.stdout.flush()
