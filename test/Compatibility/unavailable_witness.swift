// RUN: %target-typecheck-verify-swift -swift-version 3

class Foo { }

// Complain about unavailable witnesses (error in Swift 4, warning in Swift 3)
protocol P {
  func foo(bar: Foo) // expected-note{{requirement 'foo(bar:)' declared here}}
}

struct ConformsToP : P {
  @available(*, unavailable)
  func foo(bar: Foo) { } // expected-warning{{unavailable instance method 'foo(bar:)' was used to satisfy a requirement of protocol 'P'}}
}

