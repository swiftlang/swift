// RUN: %target-typecheck-verify-swift

protocol P : Sequence {
  typealias Element = Iterator.Element
  // expected-warning@-1 {{typealias overriding associated type 'Element' from protocol 'Sequence' is better expressed as same-type constraint on the protocol}}
}
