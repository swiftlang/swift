// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

// Check that ObjC associated objects are cleaned up when attached to native
// Swift objects.

class Root {
  deinit { println("deallocating root") }
}

class Associated {
  deinit { println("deallocating associated") }
}

var token: Int8 = 0

autoreleasepool {
  let root = Root()
  objc_setAssociatedObject(root, &token, Associated(),
    objc_AssociationPolicy(OBJC_ASSOCIATION_RETAIN_NONATOMIC))
}
// CHECK: deallocating root
// CHECK-NEXT: deallocating associated
