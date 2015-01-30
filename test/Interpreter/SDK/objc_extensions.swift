// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

extension NSObject {
  func frob() {
    println("I've been frobbed!")
  }

  var asHerself : NSObject {
    return self
  }

  var blackHoleWithHawkingRadiation : NSObject? {
    get {
      println("e+")
      return nil
    }
    set {
      println("e-")
    }
  }
}

var o = NSObject()

func drop(x: NSObject?) {}

// CHECK: true
println(o.respondsToSelector("frob"))
// CHECK: true
println(o.respondsToSelector("asHerself"))
// CHECK: false
println(o.respondsToSelector("setAsHerself:"))
// CHECK: true
println(o.respondsToSelector("blackHoleWithHawkingRadiation"))
// CHECK: true
println(o.respondsToSelector("setBlackHoleWithHawkingRadiation:"))

// CHECK: I've been frobbed!
o.frob()
// CHECK: true
println(o === o.asHerself)
// CHECK: e+
drop(o.blackHoleWithHawkingRadiation)
// CHECK: e-
o.blackHoleWithHawkingRadiation = NSObject()

// Use of extensions via bridging 
let str = "Hello, world"
// CHECK: I've been frobbed!
str.frob()
