// rdar://131732359 (https://github.com/swiftlang/swift/issues/71374)
// RUN: %target-typecheck-verify-swift -verify-additional-prefix assoc-
// RUN: %target-typecheck-verify-swift -verify-additional-prefix typealias- -D TYPEALIAS

#if TYPEALIAS
protocol Q {
  associatedtype A
  typealias B = A
}

func g(_ t: Any.Type) {
  if let t = t as? any Q.Type {
    _ = t.B.self // expected-typealias-error {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}
#else
protocol P {
  associatedtype A
}

func f(_ t: Any.Type) {
  if let t = t as? any P.Type {
    _ = t.A.self // expected-assoc-error {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}
#endif
