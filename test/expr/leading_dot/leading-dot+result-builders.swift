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
// expected-note@-1 {{found this candidate}}
func test<B, V, T: P, U>(_: KeyPath<B, T>, _: V.V, @TupleBuilder<B> a: () -> U) where V == T.V, V: ExpressibleByNilLiteral, V.V: Equatable {}
// expected-note@-1 {{found this candidate}}

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

test(\.value, .empty) { // expected-error {{ambiguous use of 'test(_:_:a:)'}}
  S()
}
