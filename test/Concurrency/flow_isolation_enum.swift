// RUN: %target-swift-frontend -strict-concurrency=complete -swift-version 5 -parse-as-library -emit-sil -verify %s -o /dev/null

////////////////////////
// MARK: Declarations //
////////////////////////

@globalActor
actor CustomActor {
  static let shared = CustomActor()
}

@CustomActor func requiresCustomActor() -> NonSendableType { NonSendableType() }
@MainActor func requiresMainActor() -> NonSendableType { NonSendableType() }

func randomBool() -> Bool { return false }
func logTransaction(_ i: Int) {}

enum Color: Error {
  case red
  case yellow
  case blue
}

func takeNonSendable<T>(_ ns: T) {}

@available(SwiftStdlib 5.1, *)
func takeSendable<T : Sendable>(_ s: T) {}

class NonSendableType {
  var x: Int = 0
  func f() {}
}

@available(SwiftStdlib 5.1, *)
struct SendableType: Sendable {}

struct NonSendableAOStruct<T> {
  var value: T
}

/////////////////
// MARK: Tests //
/////////////////


@available(SwiftStdlib 5.1, *)
@MainActor
enum MainActorEnumWithPayloads {
  case sendable(SendableType)
  case nonSendable(NonSendableType)

  nonisolated func trigger() {}
  nonisolated var nonisoComputed: Int { 0 }

  nonisolated init(v1: Void) {
    self = .nonSendable(NonSendableType())

    trigger() // expected-note {{after this use of 'self', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @MainActor init(v2: Void) {
    self = .nonSendable(NonSendableType())

    trigger() // expected-note {{after this use of 'self', only main actor-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in main actor-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init(v3: Void) {
    self = .nonSendable(NonSendableType())

    trigger() // expected-note {{after this use of 'self', only global actor 'CustomActor'-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in global actor 'CustomActor'-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }
}

////////////////////////////////////////////////////
// MARK: Noncopyable Enum Variants                //
////////////////////////////////////////////////////

@available(SwiftStdlib 5.1, *)
@MainActor
enum MainActorEnumWithPayloads_NC: ~Copyable {
  case sendable(SendableType)
  case nonSendable(NonSendableType)

  nonisolated func trigger() {}
  nonisolated var nonisoComputed: Int { 0 }

  nonisolated init(v1: Void) {
    self = .nonSendable(NonSendableType())

    trigger() // expected-note {{after this use of 'self', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @MainActor init(v2: Void) {
    self = .nonSendable(NonSendableType())

    trigger() // expected-note {{after this use of 'self', only main actor-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in main actor-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init(v3: Void) {
    self = .nonSendable(NonSendableType())

    trigger() // expected-note {{after this use of 'self', only global actor 'CustomActor'-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in global actor 'CustomActor'-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }
}

////////////////////////////////////////////////////
// MARK: Address Only Enum Variants               //
////////////////////////////////////////////////////

@available(SwiftStdlib 5.1, *)
@MainActor
enum MainActorEnumWithPayloads_AO<T> {
  case sendable(SendableType)
  case nonSendable(NonSendableType)
  case addressOnly(NonSendableAOStruct<T>)

  nonisolated func trigger() {}
  nonisolated var nonisoComputed: Int { 0 }

  nonisolated init(v1: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after this use of 'self', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  nonisolated init?(v1_failable: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after this use of 'self', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @MainActor init(v2: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after this use of 'self', only main actor-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in main actor-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @MainActor init?(v2_failable: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after this use of 'self', only main actor-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in main actor-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init(v3: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after this use of 'self', only global actor 'CustomActor'-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in global actor 'CustomActor'-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init?(v3_failable: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after this use of 'self', only global actor 'CustomActor'-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in global actor 'CustomActor'-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }
}

////////////////////////////////////////////////////
// MARK: Address Only Noncopyable Enum Variants   //
////////////////////////////////////////////////////

@available(SwiftStdlib 5.1, *)
@MainActor
enum MainActorEnumWithPayloads_AO_NC<T>: ~Copyable {
  case sendable(SendableType)
  case nonSendable(NonSendableType)
  case addressOnly(NonSendableAOStruct<T>)

  nonisolated func trigger() {}
  nonisolated var nonisoComputed: Int { 0 }

  nonisolated init(v1: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  nonisolated init?(v1_failable: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @MainActor init(v2: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after calling instance method 'trigger()', only main actor-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in main actor-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @MainActor init?(v2_failable: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after calling instance method 'trigger()', only main actor-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in main actor-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init(v3: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after calling instance method 'trigger()', only global actor 'CustomActor'-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in global actor 'CustomActor'-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init?(v3_failable: Void, t: sending T) {
    self = .addressOnly(NonSendableAOStruct(value: t))

    trigger() // expected-note {{after calling instance method 'trigger()', only global actor 'CustomActor'-isolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoComputed
    self = .nonSendable(NonSendableType()) // expected-warning {{cannot access 'self' here in global actor 'CustomActor'-isolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }
}
