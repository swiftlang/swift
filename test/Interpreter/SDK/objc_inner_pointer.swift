// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

class Canary: NSObject {
  deinit {
    println("died")
  }
}

var CanaryAssocObjectHandle: UInt8 = 0
// Attach an associated object with a loud deinit so we can see that the
// object died.
func hangCanary(o: AnyObject) {
  objc_setAssociatedObject(o, &CanaryAssocObjectHandle, Canary(),
                     objc_AssociationPolicy(OBJC_ASSOCIATION_RETAIN_NONATOMIC))
}

autoreleasepool {
  let data = NSData(withBytes: [2, 3, 5, 7] as UInt8[], length: 4)
  hangCanary(data)
  let bytes = UnsafePointer<UInt8>(data.bytes)
  println(bytes[0]) // CHECK:      2
  println(bytes[1]) // CHECK-NEXT: 3
  println(bytes[2]) // CHECK-NEXT: 5
  println(bytes[3]) // CHECK-NEXT: 7
} // CHECK-NEXT: died
