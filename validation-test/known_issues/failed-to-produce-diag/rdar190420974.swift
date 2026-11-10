// rdar://190420974
// RUN: %target-typecheck-verify-swift

func h(_ x: Int) -> Int? { x }
func g(_ s: String) {}
func f() {
  g(h("nope")?)
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
