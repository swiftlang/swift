// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s

public protocol MutableA {
  associatedtype Child
}

public enum A<Parent>: MutableA where Parent: MutableA {
  public typealias Child = A<Parent>
  case node(children: [Child])
}

public enum B: MutableA {
  public typealias Child = A<B>
  case node(children: [Child])
}

func test() {
  let b = B.node(children: [])
  _ = b
}

test()

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "A", {{.*}}identifier: "$s34specialized_enum_recursive_payload1AOyxGD"
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "B", {{.*}}identifier: "$s34specialized_enum_recursive_payload1BOD"
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, {{.*}}identifier: "$s34specialized_enum_recursive_payload1AOyAA1BOGD"
