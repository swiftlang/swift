// RUN: %target-swift-frontend -typecheck -disable-availability-checking -dump-ast %s | %FileCheck %s

class AnyP: P {
  init<T: P>(erasing: T) {}
}

@_typeEraser(AnyP)
protocol P {}

class ConcreteP: P {}

dynamic func opaque() -> some P {
  // CHECK: underlying_to_opaque_expr{{.*}}'some P'
  // CHECK-NEXT: call_expr implicit type='AnyP'{{.*}}arg_labels=erasing:
  // CHECK: call_expr type='ConcreteP'
  ConcreteP()
}

