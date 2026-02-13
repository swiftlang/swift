// RUN: %target-typecheck-verify-swift -solver-disable-crash-on-valid-salvage
// RUN: not --crash %target-swift-frontend -solver-enable-crash-on-valid-salvage -typecheck %s

class A {}
class B: A {}
class C: A {}

struct S {
  func foo<T: A>(types: [T.Type]) {}
}

func bar(_ s: S, _ forced_s: S!) {
  s.foo(types: [A.self, B.self]) // ok
  s.foo(types: [B.self, A.self]) // ok
  forced_s.foo(types: [A.self, B.self]) // ok
  forced_s.foo(types: [B.self, A.self]) // ok
}
