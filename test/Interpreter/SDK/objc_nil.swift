// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

var str : NSString? = nil
var url : NSURL? = nil

println("\(str == nil) \(nil == url) \(str == url)")
// CHECK: true true true

str = "abc"
url = NSURL(string: "file:///")
println("\(str == nil) \(nil == url)")
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
println(noClass == nil) // CHECK-NEXT: true
println(yesClass == nil) // CHECK-NEXT: false
println(isNilGeneric(noClass)) // CHECK-NEXT: true
println(isNilGeneric(yesClass)) // CHECK-NEXT: false

