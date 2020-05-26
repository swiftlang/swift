// RUN: %target-swift-frontend %s -O -emit-ir -g -o - | %FileCheck %s
public struct S<T> {
  let t : T
}

public let s = S<Int>(t: 0)

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "S",
// CHECK-SAME:             templateParams: ![[PARAMS:[0-9]+]], identifier:
// CHECK: ![[PARAMS]] = !{![[INTPARAM:[0-9]+]]}
// CHECK: ![[INTPARAM]] = !DITemplateTypeParameter(type: ![[INT:[0-9]+]])
// CHECK: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int",
