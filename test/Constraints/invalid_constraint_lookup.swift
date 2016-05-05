// RUN: %target-parse-verify-swift

protocol P {
  associatedtype A
  func makeIterator() -> Int
}
func f<U: P>(_ rhs: U) -> X<U.A> { // expected-error {{use of undeclared type 'X'}}
  // FIXME: This diagnostic isn't great, it happens because the generic constraint
  // 'U' from the invalid type signature never gets resolved.
  let iter = rhs.makeIterator() // expected-error {{cannot invoke 'makeIterator' with no arguments}}
}

struct Zzz<T> {
  subscript (a: Foo) -> Zzz<T> { // expected-error {{use of undeclared type 'Foo'}}
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
func insertionSort<C: Mutable> (_ elements: inout C, i: C.Index) { // expected-error {{use of undeclared type 'Mutable'}} expected-error {{'Index' is not a member type of 'C'}}
  var x: C.Iterator.Element = elements[i] // expected-error {{'Iterator' is not a member type of 'C'}}
}
