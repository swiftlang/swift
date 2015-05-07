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
    // CHECK: !DILocalVariable(tag: DW_TAG_auto_variable, name: "r", {{.*}}line: [[@LINE+2]], type: ![[SELFTY:[0-9]+]])
    // CHECK: ![[SELFTY]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_TtDC11DynamicSelf1C", {{.*}}, baseType: !"_TtC11DynamicSelf1C")
    let r = self(number: 0)
    return r
  }
}

let MoreDesignPatterns = C.Factory
