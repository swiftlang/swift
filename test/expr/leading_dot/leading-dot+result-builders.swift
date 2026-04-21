// RUN: %target-typecheck-verify-swift

protocol Element {
  associatedtype Context
}

protocol P {
  associatedtype V: P
}

@resultBuilder
struct TupleBuilder<Context> {
  static func buildBlock<E: Element>(_ t1: E) -> (E) where E.Context == Context {
    return (t1)
  }
}


func test<B, V, T: P, U>(_: KeyPath<B, T>, _: V, @TupleBuilder<B> a: () -> U) where V == T.V, V: Equatable {}
func test<B, V, T: P, U>(_: KeyPath<B, T>, _: V.V, @TupleBuilder<B> a: () -> U) where V == T.V, V: ExpressibleByNilLiteral, V.V: Equatable {}

enum E: String, P, ExpressibleByNilLiteral {
  typealias V = Self

  case empty

  init(nilLiteral: ()) { self = .empty }
}

struct S : Element {
  typealias Context = Self

  struct Inner<T: P>: P {
    typealias V = T
  }

  var value: Inner<E>

  init() { fatalError() }
}

// FIXME(diagnostics): This diagnostic is wrong. The problem is that `test` is ambiguous and should be diagnosed pre-salvage.
test(\.value, .empty) { // expected-error {{cannot infer key path type from context; consider explicitly specifying a root type}}
  S()
}
