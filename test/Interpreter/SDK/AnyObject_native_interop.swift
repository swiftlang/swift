// RUN: %target-run-simple-swift | FileCheck %s

// Test interop of pure Swift objects as ObjC objects accessed through AnyObject.

import Foundation

class FullyNative {
  deinit { println("dead") }
}

autoreleasepool {
  let c: AnyObject = FullyNative() as AnyObject

  // CHECK: (0 elements)
  println(c.description!)
}
// CHECK-NEXT: dead

