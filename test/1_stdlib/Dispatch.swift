// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Dispatch
import Foundation

// Be paranoid about the value of large 32-bit UInts on 32-bit architectures.
println(DISPATCH_PROC_EXIT)
// CHECK: 2147483648

println(DISPATCH_QUEUE_CONCURRENT.description)
// CHECK-NEXT: OS_dispatch_queue_attr

println(dispatch_data_empty.description)
// CHECK-NEXT: <>


