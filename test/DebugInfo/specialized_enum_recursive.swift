// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s

public enum A<Parent>: MutableA where Parent: MutableA {
  public var children: [A<A>] { [] }
}

public protocol MutableA {
  associatedtype Child where Child: MutableA
  var children: [Child] { get }
}

public enum B: MutableA {
  public var children: [A<B>] { [] }
}

func test() {
  let b = B.self
  _ = b
}

test()

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "A", {{.*}}identifier: "$s26specialized_enum_recursive1AOyxGD"
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "B", {{.*}}identifier: "$s26specialized_enum_recursive1BOD"
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Array", {{.*}}identifier: "$sSay26specialized_enum_recursive1AOyAA1BOGGD"
