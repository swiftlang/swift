// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// REQUIRES: objc_interop

// Test interop of pure Swift objects as ObjC objects accessed through AnyObject.

import Foundation

class FullyNative {
  deinit { print("dead") }
}

autoreleasepool {
  let c: AnyObject = FullyNative() as AnyObject

  // CHECK: {{^}}main.FullyNative{{$}}
  print(c.description!)
  _fixLifetime(c)
}
// CHECK-NEXT: dead

