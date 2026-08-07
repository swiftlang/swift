#!/usr/bin/env python3

# A small utility to take the output of a Swift validation test run
# where some compiler crashers have been fixed, and move them into the
# "fixed" testsuite, removing the "--crash" in the process.

import os
import re
import shlex
import sys


def execute_cmd(cmd):
    print(cmd)
    os.system(cmd)


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
        q_from = shlex.quote(from_filename)
        q_to = shlex.quote(to_filename)
        git_mv_cmd = 'git mv %s %s' % (q_from, q_to)
        execute_cmd(git_mv_cmd)

        # Replace "not --crash %target-swift-ide-test" with "%target-swift-ide-test".
        sed_replace_not_cmd = 'sed -e "s/not --crash %s/%s/" -i "" %s' % (
            "%target-swift-ide-test", "%target-swift-ide-test", q_to)
        execute_cmd(sed_replace_not_cmd)

        # Replace "not --crash" with "not".
        sed_replace_not_cmd = 'sed -e "s/not --crash/not/" -i "" %s' % (
            q_to)
        execute_cmd(sed_replace_not_cmd)

        # Remove "// XFAIL: whatever" lines.
        sed_remove_xfail_cmd = 'sed -e "s|^//.*XFAIL.*$||g" -i "" %s' % (
            q_to)
        execute_cmd(sed_remove_xfail_cmd)

        # Remove "// REQUIRES: asserts" lines.
        sed_remove_requires_asserts_cmd = \
            'sed -e "s|^//.*REQUIRES: asserts.*$||g" -i "" %s' % (q_to)
        execute_cmd(sed_remove_requires_asserts_cmd)

        # "git add" the result.
        git_add_cmd = 'git add %s' % (q_to)
        execute_cmd(git_add_cmd)
