#!/usr/bin/env python

# ===--- convertToJSON.py ------------------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https://swift.org/LICENSE.txt for license information
#  See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===//

# This script converts results from pre-commit benchmark tests to JSON.
# Usage: PrecommitBench_O | convertToJSON.py
#
# Input example:
#   #,TEST,SAMPLES,MIN(ms),MAX(ms),MEAN(ms),SD(ms),MEDIAN(ms)
#   1,2Sum,1,1318,1318,1318,0,1318
#   2,Ackermann,1,805,805,805,0,805
#
#   Totals,2,2123,2123,2123,0,0
#
# Output for this input:
# {
#     "Machine": {},
#     "Run": {},
#     "Tests": [
#         {
#             "Data": [
#                 1318
#             ],
#             "Info": {},
#             "Name": [
#                 "2Sum"
#             ]
#         },
#         {
#             "Data": [
#                 805
#             ],
#             "Info": {},
#             "Name": [
#                 "Ackermann"
#             ]
#         },
#         {
#             "Data": [
#                 2123
#             ],
#             "Info": {},
#             "Name": [
#                 "Totals"
#             ]
#         }
#     ]
# }

import json
import re
import sys

# Parse lines like this
# #,TEST,SAMPLES,MIN(ms),MAX(ms),MEAN(ms),SD(ms),MEDIAN(ms)
SCORERE = re.compile(r"(\d+),[ \t]*(\w+),[ \t]*([\d.]+),[ \t]*([\d.]+)")

# The Totals line would be parsed like this.
TOTALRE = re.compile(r"()(Totals),[ \t]*([\d.]+),[ \t]*([\d.]+)")
KEYGROUP = 2
VALGROUP = 4

if __name__ == "__main__":
    data = {}
    data['Tests'] = []
    data['Machine'] = {}
    data['Run'] = {}
    for line in sys.stdin:
        m = SCORERE.match(line)
        if not m:
            m = TOTALRE.match(line)
            if not m:
                continue
        test = {}
        test['Data'] = [int(m.group(VALGROUP))]
        test['Info'] = {}
        test['Name'] = [m.group(KEYGROUP)]
        data['Tests'].append(test)
    print(json.dumps(data, sort_keys=True, indent=4))
