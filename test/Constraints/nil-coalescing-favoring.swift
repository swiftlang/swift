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

do {
  class Super {}
  class Sub: Super {}

  func flatMap<R>(_: (Int) -> R?) -> R? {}

  func test() {
    let dict: Dictionary<Int, Sub>
    let sup: Super

    // CHECK: declref_expr type="(consuming Super?, @autoclosure () throws -> Super) throws -> Super" {{.*}} decl="Swift.(file).??
    let x = flatMap { dict[$0] } ?? sup // Ok
    let _: Super = x
  }
}
