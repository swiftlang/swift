// RUN: %target-typecheck-verify-swift -dump-ast 2> %t.ast
// RUN: %FileCheck %s < %t.ast

// Verify that upcasts of array literals upcast the individual elements in place
// rather than

protocol P { }
struct X : P { }

struct TakesArray<T> {
  init(_: [(T) -> Void]) { }
}

// CHECK-LABEL: func_decl "arrayUpcast(_:_:)"
// CHECK: assign_expr
// CHECK-NOT: collection_upcast_expr
// CHECK: array_expr type='[(X) -> Void]'
// CHECK-NEXT: function_conversion_expr implicit type='(X) -> Void'
// CHECK-NEXT: {{declref_expr.*x1}}
// CHECK-NEXT: function_conversion_expr implicit type='(X) -> Void'
// CHECK-NEXT: {{declref_expr.*x2}}
func arrayUpcast(_ x1: @escaping (P) -> Void, _ x2: @escaping (P) -> Void) {
  _ = TakesArray<X>([x1, x2])
}
