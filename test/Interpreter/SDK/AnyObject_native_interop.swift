// RUN: %target-run-simple-swift | FileCheck %s

// Test interop of pure Swift objects as ObjC objects accessed through AnyObject.

import Foundation

class FullyNative {
  deinit { println("dead") }
}

autoreleasepool {
  let c: AnyObject = FullyNative() as AnyObject

  // CHECK: {{.*}}11FullyNative (has 0 children)
  println(c.description!)
}
// CHECK-NEXT: dead

