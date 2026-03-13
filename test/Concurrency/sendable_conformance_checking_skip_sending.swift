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
  // expected-error@-1 {{non-Sendable type 'NonSendableKlass2' cannot be returned from main actor-isolated implementation to caller of protocol requirement 'bar'}}
  // expected-error@-2 {{non-Sendable parameter type 'NonSendableKlass2' cannot be sent from caller of protocol requirement 'bar' into main actor-isolated implementation}}
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
  // expected-error@-1 {{non-Sendable type 'NonSendableKlass4' cannot be returned from actor-isolated implementation to caller of protocol requirement 'bar'}}
  // expected-error@-2 {{non-Sendable parameter type 'NonSendableKlass4' cannot be sent from caller of protocol requirement 'bar' into actor-isolated implementation}}
}

class NonSendableKlass5 {}
// expected-note@-1 {{class 'NonSendableKlass5' does not conform to the 'Sendable' protocol}}


protocol P5 {
  func bar(_ a: sending NonSendableKlass5, _ b: NonSendableKlass5) async -> sending NonSendableKlass5
}

@MainActor
class P5Class: P5 {
  func bar(_ a: sending NonSendableKlass5, _ b: NonSendableKlass5) async -> sending NonSendableKlass5 { a }
  // expected-error@-1 {{non-Sendable parameter type 'NonSendableKlass5' cannot be sent from caller of protocol requirement 'bar' into main actor-isolated implementation}}
}

class NonSendableKlass6 {}

protocol P6 {
  func bar(_ a: sending NonSendableKlass6, _ b: sending NonSendableKlass6) async -> sending NonSendableKlass6
}

@MainActor
class P6Class: P6 {
  func bar(_ a: sending NonSendableKlass6, _ b: sending NonSendableKlass6) async -> sending NonSendableKlass6 { a }
}

class NonSendableKlass7 {}

protocol P7 {
  subscript(_: sending NonSendableKlass7) -> sending NonSendableKlass7 { get async }
}

@MainActor
struct S: P7 {
  subscript(_: sending NonSendableKlass7) -> sending NonSendableKlass7 {
    get async { NonSendableKlass7() }
  }
}
