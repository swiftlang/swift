// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

struct B {
  static var _none: B { B() }
}

struct A {
  init(_ other: B) {}
  // CHECK: constructor_decl{{.*}}interface_type="(A.Type) -> (B?) -> A"
  init(_ other: B?) {
    // CHECK: dot_syntax_call_expr type="(B) -> A"
    self.init(other ?? ._none)
  }
}
