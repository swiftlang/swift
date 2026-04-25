#!/usr/bin/env python3

# A small utility to take the output of a Swift validation test run
# where some compiler crashers have been fixed, and move them into the
# "fixed" testsuite, removing the "--crash" in the process.

import importlib
import re
import sys


def execute_cmd(cmd):
    print(cmd)
    importlib.import_module('subprocess').run(cmd)


# The regular expression we use to match compiler-crasher lines.
regex = re.compile(
    r'.*Swift(.*) :: '
    r'(compiler_crashers|compiler_crashers_2|IDE/crashers|SIL/crashers)'
    r'/(.*\.swift|.*\.sil).*')

# Take the output of lit as standard input.
for line in sys.stdin:
    match = regex.match(line)
    if match:
        suffix = match.group(2)
        filename = match.group(3)

        # Move the test over to the fixed suite.
        from_filename = 'validation-test/%s/%s' % (suffix, filename)
        to_filename = 'validation-test/%s_fixed/%s' % (suffix, filename)
        execute_cmd(['git', 'mv', from_filename, to_filename])

        # Replace "not --crash" with "not".
        execute_cmd(['sed', '-e', 's/not --crash/not/', '-i', '', to_filename])

        # Remove "// XFAIL: whatever" lines.
        execute_cmd(['sed', '-e', 's|^//.*XFAIL.*$||g', '-i', '', to_filename])

        # Remove "// REQUIRES: asserts" lines.
        execute_cmd(['sed', '-e', 's|^//.*REQUIRES: asserts.*$||g', '-i', '',
                     to_filename])

        # "git add" the result.
        execute_cmd(['git', 'add', to_filename])
