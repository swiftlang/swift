// RUN: %target-typecheck-verify-swift  -enable-experimental-feature SuppressedAssociatedTypesWithDefaults

// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults

protocol P<Primary> {
  associatedtype Primary: ~Copyable
  associatedtype Secondary: ~Copyable
}

extension P {
  func testCopyability(_ a: Self.Primary,
                       _ b: Self.Secondary) {} // expected-error {{parameter of noncopyable type 'Self.Secondary'}} // expected-note 3{{}}
}

extension P where Self.Primary: ~Copyable {
  func withNoncopyablePrimary(_ a: Self.Primary,      // expected-error {{parameter of noncopyable type 'Self.Primary'}} // expected-note 3{{}}
                              _ b: Self.Secondary) {} // expected-error {{parameter of noncopyable type 'Self.Secondary'}} // expected-note 3{{}}
}


protocol Base<Elm> { associatedtype Elm: ~Copyable } // expected-note {{'Elm' declared here}}
protocol Derived5: Base {
  associatedtype Elm: ~Copyable
  // expected-warning@-1 {{redeclaration of associated type 'Elm' from protocol 'Base' is better expressed as a 'where' clause on the protocol}}

  func check(_: Elm) // expected-error {{parameter of noncopyable type 'Self.Elm' must specify ownership}} // expected-note 3{{}}
}

protocol IterProto<Element>: ~Copyable {
  associatedtype Element: ~Copyable
}

protocol SeqOK {
  associatedtype Element: ~Copyable
  associatedtype Iterator: IterProto where Iterator.Element == Element, Iterator: ~Copyable
}


protocol TestSameTypeInverses<A>: ~Copyable {
  associatedtype A: ~Copyable

  associatedtype One: Iterable where One.Element == A

  associatedtype Two: Iterable

  func check(_ one: One.Element) // expected-error {{parameter of noncopyable type 'Self.A' must specify ownership}} // expected-note 3{{}}
  func check(_ one: Two.Element)
}
protocol Iterable<Element>: ~Copyable {
  associatedtype Element: ~Copyable
}

