# Check the various features of the ShTest format.
#
# RUN: not %{lit} -j 1 -v %{inputs}/shtest-format > %t.out
# RUN: FileCheck < %t.out %s
#
# END.

# CHECK: -- Testing:

# CHECK: PASS: shtest-format :: argv0.txt
# CHECK: FAIL: shtest-format :: external_shell/fail.txt
# CHECK-NEXT: *** TEST 'shtest-format :: external_shell/fail.txt' FAILED ***
# CHECK: Command Output (stdout):
# CHECK-NEXT: --
# CHECK-NEXT: line 1: failed test output on stdout
# CHECK-NEXT: line 2: failed test output on stdout
# CHECK: Command Output (stderr):
# CHECK-NEXT: --
# CHECK-NEXT: cat: does-not-exist: No such file or directory
# CHECK: --

# CHECK: FAIL: shtest-format :: external_shell/fail_with_bad_encoding.txt
# CHECK-NEXT: *** TEST 'shtest-format :: external_shell/fail_with_bad_encoding.txt' FAILED ***
# CHECK: Command Output (stdout):
# CHECK-NEXT: --
# CHECK-NEXT: a line with bad encoding:
# CHECK: --

# CHECK: PASS: shtest-format :: external_shell/pass.txt

# CHECK: FAIL: shtest-format :: fail.txt
# CHECK-NEXT: *** TEST 'shtest-format :: fail.txt' FAILED ***
# CHECK-NEXT: Script:
# CHECK-NEXT: --
# CHECK-NEXT: printf "line 1
# CHECK-NEXT: false
# CHECK-NEXT: --
# CHECK-NEXT: Exit Code: 1
#
# CHECK: Command Output (stdout):
# CHECK-NEXT: --
# CHECK-NEXT: Command 0: "printf"
# CHECK-NEXT: Command 0 Result: 0
# CHECK-NEXT: Command 0 Output:
# CHECK-NEXT: line 1: failed test output on stdout
# CHECK-NEXT: line 2: failed test output on stdout

# CHECK: UNRESOLVED: shtest-format :: no-test-line.txt
# CHECK: PASS: shtest-format :: pass.txt
# CHECK: UNSUPPORTED: shtest-format :: requires-missing.txt
# CHECK: PASS: shtest-format :: requires-present.txt
# CHECK: UNSUPPORTED: shtest-format :: unsupported_dir/some-test.txt
# CHECK: XFAIL: shtest-format :: xfail-feature.txt
# CHECK: XFAIL: shtest-format :: xfail-target.txt
# CHECK: XFAIL: shtest-format :: xfail.txt
# CHECK: XPASS: shtest-format :: xpass.txt
# CHECK-NEXT: *** TEST 'shtest-format :: xpass.txt' FAILED ***
# CHECK-NEXT: Script
# CHECK-NEXT: --
# CHECK-NEXT: true
# CHECK-NEXT: --
# CHECK: Testing Time

# CHECK: Unexpected Passing Tests (1)
# CHECK: shtest-format :: xpass.txt

# CHECK: Failing Tests (3)
# CHECK: shtest-format :: external_shell/fail.txt
# CHECK: shtest-format :: external_shell/fail_with_bad_encoding.txt
# CHECK: shtest-format :: fail.txt

# CHECK: Expected Passes    : 4
# CHECK: Expected Failures  : 3
# CHECK: Unsupported Tests  : 2
# CHECK: Unresolved Tests   : 1
# CHECK: Unexpected Passes  : 1
# CHECK: Unexpected Failures: 3
