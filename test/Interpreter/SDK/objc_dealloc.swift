// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

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

// Hack to build with both older and newer SDKs.
// rdar://problem/19494514
extension UInt {
  static let OBJC_ASSOCIATION_RETAIN_NONATOMIC: UInt = 1
}

autoreleasepool {
  let root = Root()
  objc_setAssociatedObject(root, &token, Associated(),
                           .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
}
// CHECK: deallocating root
// CHECK-NEXT: deallocating associated
