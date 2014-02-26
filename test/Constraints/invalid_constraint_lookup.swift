// RUN: %swift -parse %s -verify

protocol P {
  typealias A
  func generate() -> Int
}
func f<U: P>(rhs: U) -> X<U.A> { // expected-error {{use of undeclared type 'X'}}
  val g = rhs.generate() // expected-error {{'U' does not have a member named 'generate'}}
}

struct Zzz<T> {
  subscript (a: Foo) -> Zzz<T> {
  get: // expected-error {{expected '{' to start getter definition}}
  set:
    for i in value {}
  }
}

protocol _Collection {
  typealias IndexType
  typealias _Element
  subscript(i: IndexType) -> _Element {get}
}

protocol Collection : _Collection, Sequence {
  subscript(i: IndexType) -> GeneratorType.Element {get set }
}
func insertionSort<C: Mutable> (inout elements: C, i: C.IndexType) { // expected-error {{use of undeclared type 'Mutable'}} expected-error {{'IndexType' is not a member type of 'C'}}
  var x: C.GeneratorType.Element = elements[i]
}
