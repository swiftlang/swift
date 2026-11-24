// rdar://135392025
// RUN: %target-typecheck-verify-swift

struct S<T> {}

func g<T>(_ t: T) -> S<T> { S() }

func h<each T>(_ t: repeat each T) -> S<(repeat each T)> { S() }

func a<each T>(_: repeat (each T).Type) -> (repeat each T) -> S<(repeat each T)> {
  { x in g((repeat each x)) } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func b<each T>(_: repeat (each T).Type) -> (repeat each T) -> S<(repeat each T)> {
  { x in h(repeat each x) } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func f<each B, C>(_ fn: @escaping (repeat each B) -> C) -> () -> (repeat each B) -> C {
  { { b in fn(repeat each b) } } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
