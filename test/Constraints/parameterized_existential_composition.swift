// RUN: %target-typecheck-verify-swift -disable-availability-checking

func f1(_ s: any Sequence<Int> & Hashable) -> any Sequence<Int> {
  return s
}

func f2(_ s: any Sequence<Int> & Hashable) -> any Hashable {
  return s
}

func f3(_ s: any Sequence<Int> & Hashable) -> any Sequence {
  return s
}

func f4(_ s: any Sequence<Int> & Hashable) -> any Sequence & Hashable {
  return s
}

func f5(_ s: any Sequence<Int> & Hashable) -> any Sequence<Int> & Equatable {
  return s
}

func f6(_ s: any Sequence<Int> & Hashable) -> any Sequence<String> & Hashable {
  return s // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
}

func f7(_ s: any Sequence<Int> & Hashable) -> any Sequence<String> {
  return s // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
}

func f8(_ s: any Collection<Int> & Hashable) -> any Sequence<Int> & Hashable {
  return s
}

// https://github.com/swiftlang/swift/issues/71012

protocol A<T> {
  associatedtype T
}
protocol B {}
typealias C = A & B
typealias D<T> = A<T> & B

struct Foo: C {
  typealias T = Int
}

struct Bar<Value> { // expected-note {{arguments to generic parameter 'Value' ('any C' (aka 'any A & B') and 'any A<Int> & B') are expected to be equal}}
  let value: Value
}

struct Baz<U> {
  let bar: Bar<any D<U>>
}

func run() {
  let foo: any C = Foo()
  let bar = Bar(value: foo)
  _ = Baz<Int>(bar: bar)
  // expected-error@-1 {{cannot convert value of type 'Bar<any C>' (aka 'Bar<any A & B>') to expected argument type 'Bar<any A<Int> & B>'}}
}