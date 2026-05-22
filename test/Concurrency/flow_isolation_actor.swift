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
actor ActorWithMixedIsolationFields {
  @MainActor var mainField: NonSendableType
  @MainActor var mainField_trivial: Int
  @CustomActor var customField: NonSendableType // expected-note 2{{mutation of this property is only permitted within the actor}}
  @CustomActor var customField_trivial: Int
  nonisolated let nonisoField: SendableType
  var actorField: NonSendableType // expected-note 2{{mutation of this property is only permitted within the actor}}
  var actorField_trivial: Int

  nonisolated func trigger() {}

  init(v1: Void) {
    // Before decay: can assign all fields
    self.mainField = NonSendableType()
    self.mainField_trivial = 0
    self.customField = NonSendableType()
    self.customField_trivial = 0
    self.nonisoField = SendableType()
    self.actorField = NonSendableType()
    self.actorField_trivial = 0

    trigger() // expected-note 9{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    // After decay: nonisolated field is accessible, others are not
    _ = self.nonisoField // OK
    self.mainField = NonSendableType() // expected-warning {{cannot access property 'mainField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.mainField_trivial = 0 // expected-warning {{cannot access property 'mainField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    logTransaction(self.mainField_trivial) // expected-warning {{cannot access property 'mainField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.customField = NonSendableType() // expected-warning {{cannot access property 'customField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.customField_trivial = 0 // expected-warning {{cannot access property 'customField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    logTransaction(self.customField_trivial) // expected-warning {{cannot access property 'customField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.actorField = NonSendableType() // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.actorField_trivial = 0 // expected-warning {{cannot access property 'actorField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    logTransaction(self.actorField_trivial) // expected-warning {{cannot access property 'actorField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @MainActor init(v2: Void) {
    // Before decay: can assign all fields
    self.mainField = NonSendableType()
    self.mainField_trivial = 0
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.customField_trivial = 0
    self.nonisoField = SendableType()
    self.actorField = NonSendableType() // expected-warning {{actor-isolated property 'actorField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.actorField_trivial = 0

    trigger() // expected-note 9{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    // After decay: nonisolated field is accessible, others are not
    _ = self.nonisoField // OK
    self.mainField = NonSendableType() // expected-warning {{cannot access property 'mainField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.mainField_trivial = 0 // expected-warning {{cannot access property 'mainField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    logTransaction(self.mainField_trivial) // expected-warning {{cannot access property 'mainField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{cannot access property 'customField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.customField_trivial = 0 // expected-warning {{cannot access property 'customField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    logTransaction(self.customField_trivial) // expected-warning {{cannot access property 'customField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.actorField = NonSendableType() // expected-warning {{actor-isolated property 'actorField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.actorField_trivial = 0 // expected-warning {{cannot access property 'actorField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    logTransaction(self.actorField_trivial) // expected-warning {{cannot access property 'actorField_trivial' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

}

@available(SwiftStdlib 5.5, *)
actor ActorWithMixedDefaultRequirements {
  @MainActor var mainDefault: NonSendableType = requiresMainActor() // expected-note 2{{main actor-isolated default value of 'self.mainDefault' cannot be used in a global actor 'CustomActor'-isolated initializer}}
  // expected-note @-1 2{{mutation of this property is only permitted within the actor}}
  // expected-note @-2 3{{property declared here}}
  // expected-note @-3 2{{main actor-isolated default value of 'self.mainDefault' cannot be used in a nonisolated initializer}}
  var actorField: NonSendableType // expected-note 6{{mutation of this property is only permitted within the actor}}
  // expected-note @-1 6{{property declared here}}
  nonisolated let nonisoNoDefault: SendableType
  @CustomActor var customDefault: NonSendableType = requiresCustomActor() // expected-note {{mutation of this property is only permitted within the actor}}
  // expected-note @-1 3{{property declared here}}
  // expected-note @-2 4{{global actor 'CustomActor'-isolated default value of 'self.customDefault' cannot be used in a main actor-isolated initializer}}
  // expected-note @-3 4{{global actor 'CustomActor'-isolated default value of 'self.customDefault' cannot be used in a nonisolated initializer}}

  nonisolated func trigger() {}

  init(v1: Void) {
    self.nonisoNoDefault = SendableType()
    self.actorField = NonSendableType()

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 3{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.mainDefault // expected-error {{variable 'self.mainDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.actorField // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.nonisoNoDefault
    _ = self.customDefault // expected-error {{variable 'self.customDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  init(v1a: Void) {
    self.nonisoNoDefault = SendableType()
    self.actorField = NonSendableType()
    self.mainDefault = NonSendableType()

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 3{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.actorField // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.nonisoNoDefault
    _ = self.customDefault // expected-error {{variable 'self.customDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  init(v1b: Void) {
    self.nonisoNoDefault = SendableType()
    self.actorField = NonSendableType()
    self.mainDefault = NonSendableType()
    self.customDefault = NonSendableType()

    trigger() // expected-note 3{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.actorField // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.nonisoNoDefault
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @MainActor init(v2: Void) {
    self.nonisoNoDefault = SendableType()
    self.actorField = NonSendableType() // expected-warning {{actor-isolated property 'actorField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 3{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.actorField // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{actor-isolated property 'actorField' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
    _ = self.nonisoNoDefault
    _ = self.customDefault // expected-error {{variable 'self.customDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  @MainActor init(v2a: Void) {
    self.nonisoNoDefault = SendableType()
    self.actorField = NonSendableType() // expected-warning {{actor-isolated property 'actorField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.mainDefault = NonSendableType()

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 3{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.actorField // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{actor-isolated property 'actorField' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
    _ = self.nonisoNoDefault
    _ = self.customDefault // expected-error {{variable 'self.customDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  @MainActor init(v2b: Void) {
    self.nonisoNoDefault = SendableType()
    self.actorField = NonSendableType() // expected-warning {{actor-isolated property 'actorField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.mainDefault = NonSendableType()
    self.customDefault = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}

    trigger() // expected-note 3{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.actorField // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{actor-isolated property 'actorField' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
    _ = self.nonisoNoDefault
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init(v3: Void) {
    self.nonisoNoDefault = SendableType()
    self.actorField = NonSendableType() // expected-warning {{actor-isolated property 'actorField' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 3{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.mainDefault // expected-error {{variable 'self.mainDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.actorField // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{actor-isolated property 'actorField' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.nonisoNoDefault
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  @CustomActor init(v3a: Void) {
    self.nonisoNoDefault = SendableType()
    self.actorField = NonSendableType() // expected-warning {{actor-isolated property 'actorField' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    trigger() // expected-note 3{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.actorField // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{actor-isolated property 'actorField' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.nonisoNoDefault
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init(v3b: Void) {
    self.nonisoNoDefault = SendableType()
    self.actorField = NonSendableType() // expected-warning {{actor-isolated property 'actorField' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.customDefault = NonSendableType()

    trigger() // expected-note 3{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.actorField // expected-warning {{cannot access property 'actorField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{actor-isolated property 'actorField' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.nonisoNoDefault
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }
}
