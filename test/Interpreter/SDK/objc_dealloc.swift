// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

// Check that ObjC associated objects are cleaned up when attached to native
// Swift objects.

class Root {
  deinit { print("deallocating root") }
}

class Associated {
  deinit { print("deallocating associated") }
}

var token: Int8 = 0

autoreleasepool {
  let root = Root()
  objc_setAssociatedObject(root, &token, Associated(),
                           .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
}
// CHECK: deallocating root
// CHECK-NEXT: deallocating associated
