#!/usr/bin/python

# A small utility to take the output of a Swift validation test run
# where some compiler crashers have been fixed, and move them into the
# "fixed" testsuite, removing the "--crash" in the process. 

import re
import sys
import os

def execute_cmd(cmd):
    print(cmd)
    os.system(cmd)

# The regular expression we use to match compiler-crasher lines.
regex = re.compile('.*Swift :: compiler_crashers/(.*\.swift).*')

# Take the output of lit as standard input.
for line in sys.stdin:
    match = regex.match(line)
    if match:
        filename=match.group(1)

        # Move the test over to the fixed suite.
        from_filename = 'validation-test/compiler_crashers/%s' % (filename)
        to_filename = 'validation-test/compiler_crashers_fixed/%s' % (filename)
        git_mv_cmd = 'git mv %s %s' % (from_filename, to_filename)
        execute_cmd(git_mv_cmd)

        # Replace "not --crash" with "not", and remove XFAIL lines.
        sed_replace_not_cmd = 'sed -e "s/not --crash/not/" -i "" %s' % (to_filename)
        execute_cmd(sed_replace_not_cmd)

        # Remove "// XFAIL: whatever" lines.
        sed_remove_xfail_cmd = 'sed -e "s/^\\/\\/.*XFAIL.*$//g" -i "" %s' % (to_filename)
        execute_cmd(sed_remove_xfail_cmd)

        # "git add" the result.
        git_add_cmd = 'git add %s' % (to_filename)
        execute_cmd(git_add_cmd)
