// RUN: %target-typecheck-verify-swift -swift-version 6

// https://github.com/swiftlang/swift/issues/76710

class NonSendableKlass1 {}

protocol P1 {
    func bar(_ a: sending NonSendableKlass1) async -> sending NonSendableKlass1
}

@MainActor
class P1Class: P1 {
  func bar(_ a: sending NonSendableKlass1) async -> sending NonSendableKlass1 { a }
}

class NonSendableKlass2 {}
// expected-note@-1 2{{class 'NonSendableKlass2' does not conform to the 'Sendable' protocol}}

protocol P2 {
  func bar(_ a: NonSendableKlass2) async -> NonSendableKlass2
}

@MainActor
class P2Class: P2 {
  func bar(_ a: NonSendableKlass2) async -> NonSendableKlass2 { a }
  // expected-error@-1 {{non-sendable type 'NonSendableKlass2' cannot be returned from main actor-isolated implementation to caller of protocol requirement 'bar'}}
  // expected-error@-2 {{non-sendable parameter type 'NonSendableKlass2' cannot be sent from caller of protocol requirement 'bar' into main actor-isolated implementation}}
}

class NonSendableKlass3 {}

protocol P3 {
  func bar(_ a: sending NonSendableKlass3) async -> sending NonSendableKlass3
}

actor P3Actor: P3 {
  func bar(_ a: sending NonSendableKlass3) async -> sending NonSendableKlass3 { NonSendableKlass3() }
}

class NonSendableKlass4 {}
// expected-note@-1 2{{class 'NonSendableKlass4' does not conform to the 'Sendable' protocol}}

protocol P4 {
  func bar(_ a: NonSendableKlass4) async -> NonSendableKlass4
}

actor P4Actor: P4 {
  func bar(_ a: NonSendableKlass4) async -> NonSendableKlass4 { NonSendableKlass4() }
  // expected-error@-1 {{non-sendable type 'NonSendableKlass4' cannot be returned from actor-isolated implementation to caller of protocol requirement 'bar'}}
  // expected-error@-2 {{non-sendable parameter type 'NonSendableKlass4' cannot be sent from caller of protocol requirement 'bar' into actor-isolated implementation}}
}

