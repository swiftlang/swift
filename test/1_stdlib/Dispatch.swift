// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Dispatch
import Foundation

// Be paranoid about the value of large 32-bit UInts on 32-bit architectures.
print(DISPATCH_PROC_EXIT)
// CHECK: 2147483648

print(DISPATCH_QUEUE_CONCURRENT.description)
// CHECK-NEXT: OS_dispatch_queue_attr

print(dispatch_data_empty.description)
// CHECK-NEXT: <>


