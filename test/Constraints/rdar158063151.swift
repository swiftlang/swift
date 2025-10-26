// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// Make sure that the type-checker selects an `Equatable.==` instead of one from stdlib that takes _OptionalNilComparisonType

struct Value: Equatable, ExpressibleByNilLiteral {
  init(nilLiteral: ()) {
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s13rdar1580631514test1vyAA5ValueV_tF : $@convention(thin) (Value) -> ()
// CHECK: [[EQUALS_REF:%.*]] = witness_method $Value, #Equatable."==" : <Self where Self : Equatable, Self : ~Copyable, Self : ~Escapable> (Self.Type) -> (borrowing Self, borrowing Self) -> Bool
// CHECK-NEXT: apply [[EQUALS_REF]]<Value>({{.*}})
func test(v: Value) {
  _ = v == nil
}
