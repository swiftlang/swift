// rdar://192169734
// RUN: %target-typecheck-verify-swift

func g(_ e: @autoclosure () -> (any Error)?) {}

struct S {
  var id: Int
}

func f(_ s: S?) {
  g(s?.id) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
