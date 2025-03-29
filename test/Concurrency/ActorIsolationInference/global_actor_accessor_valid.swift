// RUN: %target-typecheck-verify-swift -swift-version 5

// REQUIRES: concurrency

actor SomeActor {}

@globalActor
struct GA {
  static let shared = SomeActor()
}

@globalActor
struct GGA<T> {
  static var shared: SomeActor { SomeActor() }
}

// Matching isolation.

@GA struct OnTheType {
  var observers: Int {
    @GA willSet {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
    @GA didSet {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
  }

  var property: Int {
    @GA init {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
    @GA get {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
    @GA set {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
    @GA _modify {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
  }

  subscript(subscript _: Int) -> Int {
    @GA get {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
    @GA set {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
    @GA _modify {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
  }

  var propertyRead: Int {
    @GA _read {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
  }

  subscript(subscriptRead _: Int) -> Int {
    @GA _read {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
  }
}

nonisolated struct OnTheStorage {
  @GA var property: Int {
    @GA get {}
    // expected-warning@-1 {{cannot have a global actor}}
    @GA set {}
    // expected-warning@-1 {{cannot have a global actor}}
  }

  @GA subscript(subscript _: Int) -> Int {
    @GA get {}
    // expected-warning@-1 {{cannot have a global actor}}
    @GA set {}
    // expected-warning@-1 {{cannot have a global actor}}
  }
}

@GA protocol P {}

struct FromConformance: P {
  var property: Int {
    @GA get {}
    // expected-warning@-1 {{cannot have a global actor}}
    // expected-note@-2 {{move global actor}}
  }
}
