// rdar://188839562
// RUN: %target-typecheck-verify-swift

protocol P: AnyObject {}

struct Box<T: P> {
  init(_: T.Type) {}
}

func f(_ type: any P.Type) {
  _ = Box(type) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
