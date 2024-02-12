// RUN: %target-swift-frontend -typecheck -disable-availability-checking -dump-ast -enable-experimental-opaque-type-erasure %s | %FileCheck %s

class AnyP: P {
  init<T: P>(erasing: T) {}
}

@_typeEraser(AnyP)
protocol P {}

struct ConcreteP: P, Hashable {}

// CHECK-LABEL: testTypeErased
func testTypeErased() -> some P {
  // CHECK: underlying_to_opaque_expr{{.*}}"some P"
  // CHECK-NEXT: call_expr implicit type="AnyP"
  ConcreteP()
}
