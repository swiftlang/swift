// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// Make sure that the type-checker selects an `Equatable.==` instead of one from stdlib that takes _OptionalNilComparisonType

struct Value: Equatable, ExpressibleByNilLiteral {
  init(nilLiteral: ()) {
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s13rdar1580631514test1vyAA5ValueV_tF : $@convention(thin) (Value) -> ()
// function_ref static Value.__derived_struct_equals(_:_:)
// CHECK: [[EQUALS_REF:%.*]] = function_ref @$s13rdar1580631515ValueV23__derived_struct_equalsySbAC_ACtFZ
// CHECK-NEXT: apply [[EQUALS_REF]](%0, {{.*}})
func test(v: Value) {
  _ = v == nil
}
