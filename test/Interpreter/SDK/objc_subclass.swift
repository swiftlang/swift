// RUN: %swift -sdk=%sdk -i %s | FileCheck %s
// REQUIRES: sdk

import Foundation

class SuperString : NSString {
  var len : Int
  constructor(len:Int) {
    super.constructor()
    this.len = len
  }

  func length() -> Int {
    return len
  }

  func characterAtIndex(n:Int) -> unichar {
    return unichar(0x30 + n)
  }
}

// CHECK: 0123456789
println(new SuperString(10))
// CHECK: 0123456789
println(NSString(NSString.stringWithString(new SuperString(10))))
