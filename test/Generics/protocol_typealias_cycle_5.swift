// RUN: %target-typecheck-verify-swift -warn-redundant-requirements

protocol P : Sequence {
  typealias Element = Iterator.Element
  // expected-warning@-1 {{typealias overriding associated type 'Element' from protocol 'Sequence' is better expressed as same-type constraint on the protocol}}
  // expected-warning@-2 {{redundant same-type constraint 'Self.Element' == 'Self.Iterator.Element'}}
}
