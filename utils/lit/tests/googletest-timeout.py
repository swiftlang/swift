# REQUIRES: python-psutil

# Check that the per test timeout is enforced when running GTest tests.
#
# RUN: not %{lit} -j 1 -v %{inputs}/googletest-timeout --timeout=1 > %t.cmd.out
# RUN: FileCheck < %t.cmd.out %s

# Check that the per test timeout is enforced when running GTest tests via
# the configuration file
#
# RUN: not %{lit} -j 1 -v %{inputs}/googletest-timeout \
# RUN: --param set_timeout=1 > %t.cfgset.out 2> %t.cfgset.err
# RUN: FileCheck < %t.cfgset.out %s

# CHECK: -- Testing:
# CHECK: PASS: googletest-timeout :: DummySubDir/OneTest/FirstTest.subTestA
# CHECK: TIMEOUT: googletest-timeout :: DummySubDir/OneTest/FirstTest.subTestB
# CHECK: TIMEOUT: googletest-timeout :: DummySubDir/OneTest/FirstTest.subTestC
# CHECK: Expected Passes    : 1
# CHECK: Individual Timeouts: 2

# Test per test timeout via a config file and on the command line.
# The value set on the command line should override the config file.
# RUN: not %{lit} -j 1 -v %{inputs}/googletest-timeout \
# RUN: --param set_timeout=1 --timeout=2 > %t.cmdover.out 2> %t.cmdover.err
# RUN: FileCheck < %t.cmdover.out %s
# RUN: FileCheck --check-prefix=CHECK-CMDLINE-OVERRIDE-ERR < %t.cmdover.err %s

# CHECK-CMDLINE-OVERRIDE-ERR: Forcing timeout to be 2 seconds
