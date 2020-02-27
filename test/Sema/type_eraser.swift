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

class AnyQ: Q {
  init<T: Q>(erasing: T) {}
}
@_typeEraser(AnyQ)
protocol Q {}

struct ConcreteQ: Q {}

@_functionBuilder
struct Builder {
  static func buildBlock(_ params: Q...) -> ConcreteQ {
    return ConcreteQ()
  }
}

// CHECK-LABEL: transformFnBody
@Builder
dynamic var transformFnBody: some Q {
  // CHECK: return_stmt
  // CHECK-NEXT: underlying_to_opaque_expr implicit type='some Q'
  // CHECK-NEXT: call_expr implicit type='AnyQ'{{.*}}arg_labels=erasing:
  // CHECK: declref_expr implicit type='@lvalue ConcreteQ'
  ConcreteQ()
}
