// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s

class BaseClass {
}

class GenericDerived<T>: BaseClass {
  var genericValue: T

  init(_ t: T) {
    self.genericValue = t
  }
}

func f() {
    let variable = GenericDerived<Int>(32)
}

// CHECK-DAG: ![[GENERIC_DERIVED:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "GenericDerived", {{.*}}identifier: "$s25generic_class_inheritance14GenericDerivedCyxGD")
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[GENERIC_DERIVED]], baseType: ![[BASE_CLASS:[0-9]+]]
// CHECK-DAG: ![[BASE_CLASS]] = !DICompositeType(tag: DW_TAG_structure_type, name: "BaseClass"

class GenericBase<U> {
  var u: U

  init(_ u: U) {
    self.u = u
  }
}

class GenericChild<U, V>: GenericBase<U> {
  var v: V

  init(_ u: U, _ v: V) {
    self.v = v
    super.init(u)
  }
}

func g() {
  let variable = GenericChild<Int, String>(42, "child")
}

// CHECK-DAG: ![[GENERIC_CHILD:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "GenericChild", {{.*}}identifier: "$s25generic_class_inheritance12GenericChildCyxq_GD")
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[GENERIC_CHILD]], baseType: ![[GENERIC_BASE:[0-9]+]]
// CHECK-DAG: ![[GENERIC_BASE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "GenericBase", {{.*}}identifier: "$s25generic_class_inheritance11GenericBaseCyxGD")
