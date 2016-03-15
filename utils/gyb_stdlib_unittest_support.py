# ===--- gyb_stdlib_unittest_support.py --------------*- coding: utf-8 -*-===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

TRACE = '''@autoclosure _ message: () -> String = "",
  showFrame: Bool = true,
  stackTrace: SourceLocStack = SourceLocStack(),
  file: String = #file, line: UInt = #line'''

# When the parameter list would start with a ${TRACE}, we use
# ${TRACE1} instead, to avoid the warning about an extraneous
# '_' on the first parameter.
TRACE1 = TRACE.replace(' _ ', ' ', 1)

stackTrace = 'stackTrace.pushIf(showFrame, file: file, line: line)'

trace = 'message(),\n  stackTrace: ' + stackTrace

