// RUN: %target-typecheck-verify-swift -solver-disable-diagnose-valid-salvage
// RUN: %target-typecheck-verify-swift -solver-enable-diagnose-valid-salvage -verify-additional-prefix salvage-

class A {}
class B: A {}
class C: A {}

struct S {
  func foo<T: A>(types: [T.Type]) {}
}

func bar(_ s: S, _ forced_s: S!) {
  s.foo(types: [A.self, B.self]) // ok
  s.foo(types: [B.self, A.self]) // ok
  // expected-salvage-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
  forced_s.foo(types: [A.self, B.self]) // ok
  forced_s.foo(types: [B.self, A.self]) // ok
  // expected-salvage-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
