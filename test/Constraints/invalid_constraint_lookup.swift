// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
  func makeIterator() -> Int
}
func f<U: P>(_ rhs: U) -> X<U.A> { // expected-error {{cannot find type 'X' in scope}}
  let iter = rhs.makeIterator()
}

struct Zzz<T> {
  subscript (a: Foo) -> Zzz<T> { // expected-error {{cannot find type 'Foo' in scope}}
  get: // expected-error {{expected '{' to start getter definition}}
  set:
    for i in value {}
  }
}

protocol _Collection {
  associatedtype Index
  associatedtype _Element
  subscript(i: Index) -> _Element {get}
}

protocol Collection : _Collection, Sequence {
  subscript(i: Index) -> Iterator.Element {get set }
}
func insertionSort<C: Mutable> (_ elements: inout C, i: C.Index) { // expected-error {{cannot find type 'Mutable' in scope}} expected-error {{'Index' is not a member type of type 'C'}}
  var x: C.Iterator.Element = elements[i] // expected-error {{'Iterator' is not a member type of type 'C'}}
  // expected-error@-1 {{value of type 'C' has no subscripts}}
}
