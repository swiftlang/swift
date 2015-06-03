// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

var str : NSString? = nil
var url : NSURL? = nil

print("\(str == nil) \(nil == url) \(str == url)")
// CHECK: true true true

str = "abc"
url = NSURL(string: "file:///")
print("\(str == nil) \(nil == url)")
// CHECK: false false

@inline(never)
func isNilGeneric<T>(x: T?) -> Bool {
  if let x2 = x {
    return false
  } else {
    return true
  }
}

let noClass = NSClassFromString("A Class With This Name Should Not Exist!@#!")
let yesClass = NSClassFromString("NSString")
print(noClass == nil) // CHECK-NEXT: true
print(yesClass == nil) // CHECK-NEXT: false
print(isNilGeneric(noClass)) // CHECK-NEXT: true
print(isNilGeneric(yesClass)) // CHECK-NEXT: false

