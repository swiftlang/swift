// rdar://110527003
// RUN: %target-typecheck-verify-swift

func uncurry<each A, each B, C>(_ fn: @escaping (repeat each A) -> (repeat each B) -> C) -> (repeat each A, repeat each B) -> C {
  return { a, b in fn(repeat each a)(repeat each b) } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
