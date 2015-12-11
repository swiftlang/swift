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
print(o.respondsTo("frob"))
// CHECK: true
print(o.respondsTo("asHerself"))
// CHECK: false
print(o.respondsTo("setAsHerself:"))
// CHECK: true
print(o.respondsTo("blackHoleWithHawkingRadiation"))
// CHECK: true
print(o.respondsTo("setBlackHoleWithHawkingRadiation:"))

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
