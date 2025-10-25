// RUN: %target-swift-frontend -dump-ast -swift-version 6 %s | %FileCheck %s

// CHECK: (func_decl decl_context={{.*}} range={{.*}} "test(v:)"
func test<T: Equatable>(v: T) {
  // CHECK: (function_conversion_expr implicit type="@Sendable (T.Type) -> (borrowing T, borrowing T) -> Bool"
  _ = v == v
}
