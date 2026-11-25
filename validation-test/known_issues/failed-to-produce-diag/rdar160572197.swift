// rdar://160572197
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

protocol P<V> {
  associatedtype V
}

struct S<each U: P>: P {
  typealias V = (repeat (each U).V)
}

func combine<each C: P>(_ c: repeat each C) -> some P<(repeat (each C).V)> {
  S<repeat each C>() // expected-error {{pack expansion requires that 'S<repeat each C>.V' (aka 'repeat each C.V') and 'each C' have the same shape}}
}

struct M<V>: P {}

extension P {
  func map<T>(_ fn: @escaping (V) -> T) -> some P<T> { M<T>() }
}

struct A: P { typealias V = Int }

let x = combine(A(), A()).map { $0.0 + $0.1 } // expected-error {{pack reference 'A.V, A.V' (aka 'Int, Int') can only appear in pack expansion}}

func combineS<each C: P>(_ c: repeat each C) -> S<repeat each C> {
  S<repeat each C>()
}

let y = combineS(A(), A()).map { $0.0 + $0.1 } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
