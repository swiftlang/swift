#!/usr/bin/env python

# The following script takes as input a SIL fragment and changes all
# SSA variables into FileCheck variables. This significantly reduces
# the amount of time required for creating complicated FileCheck
# Tests.

import argparse
import re
import sys
import textwrap

parser = argparse.ArgumentParser(description=textwrap.dedent("""
Takes an input SIL fragment and changes all SSA variables into FileCheck
variables.
"""))

parser.add_argument('input', type=argparse.FileType('r'),
                    help='Input file. \'-\' for stdin.')
parser.add_argument('-o', type=argparse.FileType('w'),
                    metavar='output',
                    help='Output file. Defaults to stdout.',
                    default=sys.stdout)

args = parser.parse_args()

seen_variables = set([])
ssa_re = re.compile('[%](\d+)')
for l in args.input.readlines():
    l = l[:l.find('//')].rstrip() + "\n"
    have_match = False
    for match in ssa_re.finditer(l):
        have_match = True
        var = match.groups()[0]
        if var not in seen_variables:
            l = l.replace('%' + var, '[[VAR_%s:%%[0-9]+]]' % var)
            seen_variables.add(var)
        else:
            l = l.replace('%' + var, '[[VAR_%s]]' % var)
    if have_match:
        l = '// CHECK: ' + l
    args.o.write(l)
