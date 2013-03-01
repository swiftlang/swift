// RUN: %swift -sdk=%sdk -i %s | FileCheck %s
// REQUIRES: sdk

import Foundation

extension NSObject {
  func frob() {
    println("I've been frobbed!")
  }

  var asHerself : NSObject {
    return this
  }

  var blackHoleWithHawkingRadiation : NSObject {
  get:
    println("e+")
    return nil
  set:
    println("e-")
  }
}

var o = new NSObject()

func drop(x:NSObject) {}

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
o.blackHoleWithHawkingRadiation = new NSObject()
