// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

class C {
  let n : Int64
  required init (number i :Int64) {
    self.n = i
  }
}

extension C {
  class func Factory() -> Self {
    // Currently we emit the static type C for r.
    // CHECK: ![[BASE:.*]] = !DICompositeType({{.*}}identifier: "$s11DynamicSelf1CCD"
    // CHECK: !DILocalVariable(name: "r",
    // CHECK-SAME:             line: [[@LINE+4]], type: ![[SELFTY:[0-9]+]])
    // CHECK: ![[SELFTY]] = !DIDerivedType(tag: DW_TAG_typedef,
    // CHECK-SAME:                         name: "$s11DynamicSelf1CCXDD",
    // CHECK-SAME:                         baseType: ![[BASE]])
    let r = self.init(number: 0)
    return r
  }
}

let MoreDesignPatterns = C.Factory
