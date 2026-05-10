// RUN: %target-swift-frontend -emit-ir %s -g -module-name closure | %FileCheck %s

// CHECK-DAG: !{{[0-9]+}} = !DICompositeType(tag: DW_TAG_structure_type, name: "$sxxQp_QSiIgp_D", {{.*}})
public func f<each Input>(builder: (repeat each Input) -> ()) {}

public protocol P {
  func f() -> Self
}

// CHECK-DAG: !{{[0-9]+}} = !DICompositeType(tag: DW_TAG_structure_type, name: "$sxxQp_tz_xxQp_QP_Rvz7closure1PRzlXXD", {{.*}})
public func foo<each T: P>(t: repeat each T) -> () -> () {
  var x = (repeat each t)
  return { x = (repeat (each x).f()) }
}
