// RUN: %target-run-simple-swift | FileCheck %s

import Dispatch

// Be paranoid about the value of large 32-bit UInts on 32-bit architectures.
println(DISPATCH_PROC_EXIT)
// CHECK: 2147483648
