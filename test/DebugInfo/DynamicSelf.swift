// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

class C {
  let n : Int
  required init (number i :Int) {
    self.n = i
  }
}

extension C {
  class func Factory() -> Self {
    // Currently we emit the static type C for r.
    // CHECK: [ DW_TAG_auto_variable ] [r] [line [[@LINE+1]]]
    let r = self(number: 0)
    return r
  }
}

let MoreDesignPatterns = C.Factory
