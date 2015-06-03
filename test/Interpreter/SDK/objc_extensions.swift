// RUN: %target-run-simple-swift | FileCheck %s
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

func drop(x: NSObject?) {}

// CHECK: true
print(o.respondsToSelector("frob"))
// CHECK: true
print(o.respondsToSelector("asHerself"))
// CHECK: false
print(o.respondsToSelector("setAsHerself:"))
// CHECK: true
print(o.respondsToSelector("blackHoleWithHawkingRadiation"))
// CHECK: true
print(o.respondsToSelector("setBlackHoleWithHawkingRadiation:"))

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
