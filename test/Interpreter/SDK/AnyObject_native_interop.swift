// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

// Test interop of pure Swift objects as ObjC objects accessed through AnyObject.

import Foundation

class FullyNative {
  deinit { println("dead") }
}

autoreleasepool {
  let c: AnyObject = FullyNative() as AnyObject

  // CHECK: {{^}}main.FullyNative{{$}}
  println(c.description!)
}
// CHECK-NEXT: dead

