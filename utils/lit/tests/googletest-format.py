# Check the various features of the GoogleTest format.
#
# RUN: not %{lit} -j 1 -v %{inputs}/googletest-format > %t.out
# RUN: FileCheck < %t.out %s
#
# END.

# CHECK: -- Testing:
# CHECK: PASS: googletest-format :: DummySubDir/OneTest/FirstTest.subTestA
# CHECK: FAIL: googletest-format :: DummySubDir/OneTest/FirstTest.subTestB
# CHECK-NEXT: *** TEST 'googletest-format :: DummySubDir/OneTest/FirstTest.subTestB' FAILED ***
# CHECK-NEXT: I am subTest B, I FAIL
# CHECK-NEXT: And I have two lines of output
# CHECK: ***
# CHECK: PASS: googletest-format :: DummySubDir/OneTest/ParameterizedTest/0.subTest
# CHECK: PASS: googletest-format :: DummySubDir/OneTest/ParameterizedTest/1.subTest
# CHECK: Failing Tests (1)
# CHECK: Expected Passes    : 3
# CHECK: Unexpected Failures: 1

