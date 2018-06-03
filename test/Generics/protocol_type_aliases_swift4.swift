// RUN: %target-typecheck-verify-swift -typecheck %s -verify -swift-version 4

protocol P1 {
  typealias A = Int
  // expected-warning@-1 {{typealias should not be declared within a protocol; move to an extension or use 'associatedtype' to define an associated type requirement}} {{3-12=associatedtype}}
}
protocol P2 : P1 {
  associatedtype A
}

protocol P3 {
  associatedtype A // expected-note 2 {{'A' declared here}}
}

// We leave the current warning intact when we 'override' an associatedtype in Swift 4
protocol P4: P3 {
  typealias A = Int
  // expected-warning@-1 {{typealias overriding associated type 'A' from protocol 'P3' is better expressed as same-type constraint on the protocol}} {{16-16= where A == Int}} {{3-20=}}
}

// Nothing changes here
protocol P5: P3 {
  associatedtype A: P1
  // expected-warning@-1 {{redeclaration of associated type 'A' from protocol 'P3' is better expressed as a 'where' clause on the protocol}} {{16-16= where A: P1}} {{3-23=}}
}