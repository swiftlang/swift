// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

// Test that a bound generic type is fully resolved in the debug info.

public protocol P {}

public struct S : P {
  var x: Int
}
// This is significant, it must be bound to S:                    main.BoundGeneric<main.S>
// CHECK-DAG: ![[S:[0-9]+]] = !DICompositeType({{.*}}identifier: "$s4main12BoundGenericVyAA1SVGD")

public extension BoundGeneric where T == S {
  func f() {
// CHECK-DAG: !DILocalVariable(name: "self", arg: 1,{{.*}} line: [[@LINE-1]],{{.*}} type: ![[C_BGS:[0-9]+]],
// CHECK-DAG: ![[C_BGS]] = !DIDerivedType(tag: DW_TAG_const_type,{{.*}} baseType: ![[BGS:[0-9]+]])
// CHECK-DAG: ![[BGS]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}} elements: ![[ELTS:[0-9]+]],
// CHECK-DAG: ![[ELTS]] = !{![[MEMBER:[0-9]+]]}
// CHECK-DAG: ![[MEMBER]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[S]],
  }
}

public struct BoundGeneric<T> where T : P {
  let x : T
}

public let pat = BoundGeneric<S>(x: S(x: 23))
pat.f()
