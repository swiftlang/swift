// RUN: %target-run-simple-swift-swift3 | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

extension NSObject {
  func frob() {
    print("I've been frobbed!")
  }

  var asHerself : NSObject {
    return self
  }

  var blackHoleWithHawkingRadiation : NSObject? {
    get {
      print("e+")
      return nil
    }
    set {
      print("e-")
    }
  }
}

var o = NSObject()

func drop(_ x: NSObject?) {}

// CHECK: true
print(o.responds(to: "frob"))
// CHECK: true
print(o.responds(to: "asHerself"))
// CHECK: false
print(o.responds(to: "setAsHerself:"))
// CHECK: true
print(o.responds(to: "blackHoleWithHawkingRadiation"))
// CHECK: true
print(o.responds(to: "setBlackHoleWithHawkingRadiation:"))

// Test #selector for referring to methods.
// CHECK: true
print(o.responds(to: #selector(NSObject.frob)))

// CHECK: I've been frobbed!
o.frob()
// CHECK: true
print(o === o.asHerself)
// CHECK: e+
drop(o.blackHoleWithHawkingRadiation)
// CHECK: e-
o.blackHoleWithHawkingRadiation = NSObject()

// Use of extensions via bridging 
let str = "Hello, world"
// CHECK: I've been frobbed!
str.frob()
