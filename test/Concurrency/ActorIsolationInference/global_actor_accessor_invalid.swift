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

// Mismatching isolation.

// Accessor vs. type context.
do {
  nonisolated struct S {
    var observers: Int {
      @GA willSet {}
      // expected-error@-1:11 {{global actor 'GA'-isolated willSet observer for 'observers' must match the isolation of the implicitly nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
      @GA didSet {}
      // expected-error@-1:11 {{global actor 'GA'-isolated didSet observer for 'observers' must match the isolation of the implicitly nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
    }

    var property: Int {
      @GA init {}
      // expected-error@-1:11 {{global actor 'GA'-isolated init acecssor for 'property' must match the isolation of the implicitly nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
      @GA get {}
      // expected-error@-1:11 {{global actor 'GA'-isolated getter for 'property' must match the isolation of the implicitly nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
      @GA set {}
      // expected-error@-1:11 {{global actor 'GA'-isolated setter for 'property' must match the isolation of the implicitly nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
      @GA _modify {}
      // expected-error@-1:11 {{global actor 'GA'-isolated _modify accessor for 'property' must match the isolation of the implicitly nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
    }

    subscript(subscript _: Int) -> Int {
      @GA get {}
      // expected-error@-1:11 {{global actor 'GA'-isolated getter for 'subscript(subscript:)' must match the isolation of the implicitly nonisolated subscript}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
      @GA set {}
      // expected-error@-1:11 {{global actor 'GA'-isolated setter for 'subscript(subscript:)' must match the isolation of the implicitly nonisolated subscript}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
      @GA _modify {}
      // expected-error@-1:11 {{global actor 'GA'-isolated _modify accessor for 'subscript(subscript:)' must match the isolation of the implicitly nonisolated subscript}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
    }

    var propertyRead: Int {
      @GA _read {}
      // expected-error@-1:11 {{global actor 'GA'-isolated _read accessor for 'propertyRead' must match the isolation of the implicitly nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
    }

    subscript(subscriptRead _: Int) -> Int {
      @GA _read {}
      // expected-error@-1:11 {{global actor 'GA'-isolated _read accessor for 'subscript(subscriptRead:)' must match the isolation of the implicitly nonisolated subscript}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
    }
  }
}

// Accessor vs. storage.
do {
  nonisolated struct S {
    nonisolated var nonisolatedProperty: Int {
      @GA get {}
      // expected-error@-1:11 {{global actor 'GA'-isolated getter for 'nonisolatedProperty' must match the isolation of the nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
    }

    nonisolated subscript(nonisolatedSubscript _: Int) -> Int {
      @GA get {}
      // expected-error@-1:11 {{global actor 'GA'-isolated getter for 'subscript(nonisolatedSubscript:)' must match the isolation of the nonisolated subscript}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
    }

    @GGA<Int> var isolatedProperty: Int {
      @GGA<Bool> get {}
      // expected-error@-1:18 {{global actor 'GGA<Bool>'-isolated getter for 'isolatedProperty' must match the isolation of the global actor 'GGA<Int>'-isolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
    }

    @GGA<Int> subscript(isolatedSubscript _: Int) -> Int {
      @GGA<Bool> get {}
      // expected-error@-1:18 {{global actor 'GGA<Bool>'-isolated getter for 'subscript(isolatedSubscript:)' must match the isolation of the global actor 'GGA<Int>'-isolated subscript}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
    }
  }
}

// Accessor vs. conformance.
do {
  @GGA<Void> protocol P {}

  struct S: P {
    var property: Int {
      @GA get {}
      // expected-error@-1:11 {{global actor 'GA'-isolated getter for 'property' must match the isolation of the implicitly global actor 'GGA<Void>'-isolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
    }
  }
}

// Accessor requirement vs. protocol.
do {
  protocol P {
    var property: Int {
      @GA get
      // expected-error@-1:11 {{global actor 'GA'-isolated getter for 'property' must match the isolation of the implicitly nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
      @GA set
      // expected-error@-1:11 {{global actor 'GA'-isolated setter for 'property' must match the isolation of the implicitly nonisolated property}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
    }

    subscript(_: Int) -> Int {
      @GA get
      // expected-error@-1:11 {{global actor 'GA'-isolated getter for 'subscript(_:)' must match the isolation of the implicitly nonisolated subscript}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
      @GA set
      // expected-error@-1:11 {{global actor 'GA'-isolated setter for 'subscript(_:)' must match the isolation of the implicitly nonisolated subscript}}{{none}}
      // expected-warning@-2 {{cannot have a global actor}}
      // expected-note@-3 {{move global actor}}
    }
  }
}
