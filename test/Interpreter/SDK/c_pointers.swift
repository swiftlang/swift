// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift %s -Xfrontend -enable-c-function-pointers -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation
#if os(OSX)
import AppKit
typealias XXColor = NSColor
#endif
#if os(iOS) || os(tvOS)
import UIKit
typealias XXColor = UIColor
#endif


// Exercise some common APIs that make use of C pointer arguments.

//
// Typed C pointers
//

let rgb = CGColorSpaceCreateDeviceRGB()
let cgRed = CGColorCreate(rgb, [1.0, 0.0, 0.0, 1.0])

let nsRed = XXColor(CGColor: cgRed)

var r: CGFloat = 0.5, g: CGFloat = 0.5, b: CGFloat = 0.5, a: CGFloat = 0.5
#if os(OSX)
nsRed!.getRed(&r, green: &g, blue: &b, alpha: &a)
#else
nsRed.getRed(&r, green: &g, blue: &b, alpha: &a)
#endif

// CHECK-LABEL: Red is:
print("Red is:")
print("<\(r) \(g) \(b) \(a)>") // CHECK-NEXT: <1.0 0.0 0.0 1.0>

//
// Void C pointers
//

// FIXME: Array type annotation should not be required
let data = NSData(bytes: [1.5, 2.25, 3.125] as [Double], 
                  length: sizeof(Double.self) * 3)
var fromData = [0.25, 0.25, 0.25]
let notFromData = fromData
data.getBytes(&fromData, length: sizeof(Double.self) * 3)

// CHECK-LABEL: Data is:
print("Data is:")
print(fromData[0]) // CHECK-NEXT: 1.5
print(fromData[1]) // CHECK-NEXT: 2.25
print(fromData[2]) // CHECK-NEXT: 3.125

// CHECK-LABEL: Independent data is:
print("Independent data is:")
print(notFromData[0]) // CHECK-NEXT: 0.25
print(notFromData[1]) // CHECK-NEXT: 0.25
print(notFromData[2]) // CHECK-NEXT: 0.25

//
// ObjC pointers
//

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
// error died.
func hangCanary(o: AnyObject) {
  objc_setAssociatedObject(o, &CanaryAssocObjectHandle, Canary(),
                           .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
}

// CHECK-LABEL: NSError out:
print("NSError out:")
autoreleasepool {
  do {
    let s = try NSString(contentsOfFile: "/hopefully/does/not/exist\u{1B}",
                         encoding: NSUTF8StringEncoding)
    _preconditionFailure("file should not actually exist")
  } catch {
    print(error._code) // CHECK-NEXT: 260
    hangCanary(error as NSError)
  }
}
// The result error should have died with the autorelease pool
// CHECK-NEXT: died
class DumbString: NSString {
  override func characterAtIndex(x: Int) -> unichar { _preconditionFailure("nope") }
  override var length: Int { return 0 }

  convenience init(contentsOfFile s: String, encoding: NSStringEncoding) throws {
    self.init()
    throw NSError(domain: "Malicious Mischief", code: 594, userInfo: nil)
  }
}

// CHECK-LABEL: NSError in:
print("NSError in:")
autoreleasepool {
  do {
    try DumbString(contentsOfFile: "foo", encoding: NSUTF8StringEncoding)
  } catch {
    print(error._domain) // CHECK-NEXT: Malicious Mischief
    print(error._code) // CHECK-NEXT: 594
    hangCanary(error as NSError)
  }
}
// The result error should have died with the autorelease pool
// CHECK-NEXT: died

let s = "Hello World"
puts(s)
// CHECK-NEXT: Hello World

//
// C function pointers
//

var unsorted = [3, 14, 15, 9, 2, 6, 5]
qsort(&unsorted, unsorted.count, sizeofValue(unsorted[0])) { a, b in
  return Int32(UnsafePointer<Int>(a).memory - UnsafePointer<Int>(b).memory)
}
// CHECK-NEXT: [2, 3, 5, 6, 9, 14, 15]
print(unsorted)
