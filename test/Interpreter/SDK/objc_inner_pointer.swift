// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

class Canary: NSObject {
  deinit {
    print("died")
  }
}

var CanaryAssocObjectHandle: UInt8 = 0


// Hack to build with both older and newer SDKs.
// rdar://problem/19494514
extension UInt {
  static let OBJC_ASSOCIATION_RETAIN_NONATOMIC: UInt = 1
}

// Attach an associated object with a loud deinit so we can see that the
// object died.
func hangCanary(o: AnyObject) {
  objc_setAssociatedObject(o, &CanaryAssocObjectHandle, Canary(),
                           .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
}

// CHECK-LABEL: NSData:
print("NSData:")
autoreleasepool {
  var bytes: UnsafeMutablePointer<UInt8>
  repeat {
    let data = NSData(bytes: [2, 3, 5, 7] as [UInt8], length: 4)
    hangCanary(data)
    bytes = UnsafeMutablePointer<UInt8>(data.bytes)
  } while false // CHECK-NOT: died
  print(bytes[0]) // CHECK:      2
  print(bytes[1]) // CHECK-NEXT: 3
  print(bytes[2]) // CHECK-NEXT: 5
  print(bytes[3]) // CHECK-NEXT: 7
} // CHECK-NEXT: died

// CHECK-LABEL: AnyObject:
print("AnyObject:")
autoreleasepool {
  var bytes: UnsafeMutablePointer<UInt8>
  repeat {
    let data = NSData(bytes: [11, 13, 17, 19] as [UInt8], length: 4)
    hangCanary(data)
    let dataAsAny: AnyObject = data
    bytes = UnsafeMutablePointer<UInt8>(dataAsAny.bytes!)
  } while false // CHECK-NOT: died
  print(bytes[0]) // CHECK:      11
  print(bytes[1]) // CHECK-NEXT: 13
  print(bytes[2]) // CHECK-NEXT: 17
  print(bytes[3]) // CHECK-NEXT: 19
} // CHECK-NEXT: died
