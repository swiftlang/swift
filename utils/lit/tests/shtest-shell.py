# Check the internal shell handling component of the ShTest format.
#
# RUN: not %{lit} -j 1 -v %{inputs}/shtest-shell > %t.out
# RUN: FileCheck < %t.out %s
#
# END.

# CHECK: -- Testing:

# CHECK: FAIL: shtest-shell :: error-0.txt
# CHECK: *** TEST 'shtest-shell :: error-0.txt' FAILED ***
# CHECK: Command 0: "not-a-real-command"
# CHECK: Command 0 Result: 127
# CHECK: Command 0 Stderr:
# CHECK: 'not-a-real-command': command not found
# CHECK: ***

# FIXME: The output here sucks.
#
# CHECK: FAIL: shtest-shell :: error-1.txt
# CHECK: *** TEST 'shtest-shell :: error-1.txt' FAILED ***
# CHECK: shell parser error on: 'echo "missing quote'
# CHECK: ***

# CHECK: FAIL: shtest-shell :: error-2.txt
# CHECK: *** TEST 'shtest-shell :: error-2.txt' FAILED ***
# CHECK: Unsupported redirect:
# CHECK: ***

# CHECK: PASS: shtest-shell :: redirects.txt
# CHECK: PASS: shtest-shell :: sequencing-0.txt
# CHECK: XFAIL: shtest-shell :: sequencing-1.txt
# CHECK: Failing Tests (3)
