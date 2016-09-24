# REQUIRES: python-psutil

# Test per test timeout using external shell
# RUN: not %{lit} \
# RUN: %{inputs}/shtest-timeout/infinite_loop.py \
# RUN: %{inputs}/shtest-timeout/quick_then_slow.py \
# RUN: %{inputs}/shtest-timeout/short.py \
# RUN: %{inputs}/shtest-timeout/slow.py \
# RUN: -j 1 -v --debug --timeout 1 --param external=1 > %t.extsh.out 2> %t.extsh.err
# RUN: FileCheck --check-prefix=CHECK-OUT-COMMON < %t.extsh.out %s
# RUN: FileCheck --check-prefix=CHECK-EXTSH-ERR < %t.extsh.err %s
#
# CHECK-EXTSH-ERR: Using external shell

# Test per test timeout using internal shell
# RUN: not %{lit} \
# RUN: %{inputs}/shtest-timeout/infinite_loop.py \
# RUN: %{inputs}/shtest-timeout/quick_then_slow.py \
# RUN: %{inputs}/shtest-timeout/short.py \
# RUN: %{inputs}/shtest-timeout/slow.py \
# RUN: -j 1 -v --debug --timeout 1 --param external=0 > %t.intsh.out 2> %t.intsh.err
# RUN: FileCheck  --check-prefix=CHECK-OUT-COMMON < %t.intsh.out %s
# RUN: FileCheck --check-prefix=CHECK-INTSH-OUT < %t.intsh.out %s
# RUN: FileCheck --check-prefix=CHECK-INTSH-ERR < %t.intsh.err %s
#
# CHECK-INTSH-OUT: TIMEOUT: per_test_timeout :: infinite_loop.py
# CHECK-INTSH-OUT: Command 0 Reached Timeout: True
# CHECK-INTSH-OUT: Command 0 Output:
# CHECK-INTSH-OUT-NEXT: Running infinite loop


# CHECK-INTSH-OUT: TIMEOUT: per_test_timeout :: quick_then_slow.py
# CHECK-INTSH-OUT: Timeout: Reached timeout of 1 seconds
# CHECK-INTSH-OUT: Command Output
# CHECK-INTSH-OUT: Command 0 Reached Timeout: False
# CHECK-INTSH-OUT: Command 0 Output:
# CHECK-INTSH-OUT-NEXT: Running in quick mode
# CHECK-INTSH-OUT: Command 1 Reached Timeout: True
# CHECK-INTSH-OUT: Command 1 Output:
# CHECK-INTSH-OUT-NEXT: Running in slow mode

# CHECK-INTSH-OUT: TIMEOUT: per_test_timeout :: slow.py
# CHECK-INTSH-OUT: Command 0 Reached Timeout: True
# CHECK-INTSH-OUT: Command 0 Output:
# CHECK-INTSH-OUT-NEXT: Running slow program

# CHECK-INTSH-ERR: Using internal shell

# Test per test timeout set via a config file rather than on the command line
# RUN: not %{lit} \
# RUN: %{inputs}/shtest-timeout/infinite_loop.py \
# RUN: %{inputs}/shtest-timeout/quick_then_slow.py \
# RUN: %{inputs}/shtest-timeout/short.py \
# RUN: %{inputs}/shtest-timeout/slow.py \
# RUN: -j 1 -v --debug --param external=0 \
# RUN: --param set_timeout=1 > %t.cfgset.out 2> %t.cfgset.err
# RUN: FileCheck --check-prefix=CHECK-OUT-COMMON  < %t.cfgset.out %s
# RUN: FileCheck --check-prefix=CHECK-CFGSET-ERR < %t.cfgset.err %s
#
# CHECK-CFGSET-ERR: Using internal shell

# CHECK-OUT-COMMON: TIMEOUT: per_test_timeout :: infinite_loop.py
# CHECK-OUT-COMMON: Timeout: Reached timeout of 1 seconds
# CHECK-OUT-COMMON: Command {{([0-9]+ )?}}Output
# CHECK-OUT-COMMON: Running infinite loop

# CHECK-OUT-COMMON: TIMEOUT: per_test_timeout :: quick_then_slow.py
# CHECK-OUT-COMMON: Timeout: Reached timeout of 1 seconds
# CHECK-OUT-COMMON: Command {{([0-9]+ )?}}Output
# CHECK-OUT-COMMON: Running in quick mode
# CHECK-OUT-COMMON: Running in slow mode

# CHECK-OUT-COMMON: PASS: per_test_timeout :: short.py

# CHECK-OUT-COMMON: TIMEOUT: per_test_timeout :: slow.py
# CHECK-OUT-COMMON: Timeout: Reached timeout of 1 seconds
# CHECK-OUT-COMMON: Command {{([0-9]+ )?}}Output
# CHECK-OUT-COMMON: Running slow program

# CHECK-OUT-COMMON: Expected Passes{{ *}}: 1
# CHECK-OUT-COMMON: Individual Timeouts{{ *}}: 3

# Test per test timeout via a config file and on the command line.
# The value set on the command line should override the config file.
# RUN: not %{lit} \
# RUN: %{inputs}/shtest-timeout/infinite_loop.py \
# RUN: %{inputs}/shtest-timeout/quick_then_slow.py \
# RUN: %{inputs}/shtest-timeout/short.py \
# RUN: %{inputs}/shtest-timeout/slow.py \
# RUN: -j 1 -v --debug --param external=0 \
# RUN: --param set_timeout=1 --timeout=2 > %t.cmdover.out 2> %t.cmdover.err
# RUN: FileCheck --check-prefix=CHECK-CMDLINE-OVERRIDE-OUT  < %t.cmdover.out %s
# RUN: FileCheck --check-prefix=CHECK-CMDLINE-OVERRIDE-ERR < %t.cmdover.err %s

# CHECK-CMDLINE-OVERRIDE-ERR: Forcing timeout to be 2 seconds

# CHECK-CMDLINE-OVERRIDE-OUT: TIMEOUT: per_test_timeout :: infinite_loop.py
# CHECK-CMDLINE-OVERRIDE-OUT: Timeout: Reached timeout of 2 seconds
# CHECK-CMDLINE-OVERRIDE-OUT: Command {{([0-9]+ )?}}Output
# CHECK-CMDLINE-OVERRIDE-OUT: Running infinite loop

# CHECK-CMDLINE-OVERRIDE-OUT: TIMEOUT: per_test_timeout :: quick_then_slow.py
# CHECK-CMDLINE-OVERRIDE-OUT: Timeout: Reached timeout of 2 seconds
# CHECK-CMDLINE-OVERRIDE-OUT: Command {{([0-9]+ )?}}Output
# CHECK-CMDLINE-OVERRIDE-OUT: Running in quick mode
# CHECK-CMDLINE-OVERRIDE-OUT: Running in slow mode

# CHECK-CMDLINE-OVERRIDE-OUT: PASS: per_test_timeout :: short.py

# CHECK-CMDLINE-OVERRIDE-OUT: TIMEOUT: per_test_timeout :: slow.py
# CHECK-CMDLINE-OVERRIDE-OUT: Timeout: Reached timeout of 2 seconds
# CHECK-CMDLINE-OVERRIDE-OUT: Command {{([0-9]+ )?}}Output
# CHECK-CMDLINE-OVERRIDE-OUT: Running slow program

# CHECK-CMDLINE-OVERRIDE-OUT: Expected Passes{{ *}}: 1
# CHECK-CMDLINE-OVERRIDE-OUT: Individual Timeouts{{ *}}: 3
