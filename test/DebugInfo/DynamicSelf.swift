// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

class C {
  let n : Int64
  required init (number i :Int64) {
    self.n = i
  }
}

extension C {
  class func Factory() -> Self {
    // Currently we emit the static type C for r.
    // CHECK-DAG: !DILocalVariable(name: "r", {{.*}}line: [[@LINE+2]], type: ![[SELFTY:[0-9]+]])
    // CHECK-DAG: ![[SELFTY]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_TtDC11DynamicSelf1C", {{.*}}, baseType: ![[SELFBT:[0-9]+]])
    // CHECK-DAG: ![[SELFBT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "C", {{.*}}, identifier: "_TtC11DynamicSelf1C")
    let r = self.init(number: 0)
    return r
  }
}

let MoreDesignPatterns = C.Factory
