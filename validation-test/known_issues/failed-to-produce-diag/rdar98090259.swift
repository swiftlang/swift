// rdar://98090259
// RUN: %target-typecheck-verify-swift

func singleArg<A, B>(_ x: A, _ lambda: (A) -> B) -> B {
  lambda(x)
}

func foo() {
  let _ = singleArg(1) { a in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    singleArg(a) { (a, b) in a + b }
  }
}
