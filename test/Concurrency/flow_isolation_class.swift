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
class MainActorClassWithMixedFields {
  nonisolated let nonisoField: SendableType
  @CustomActor var customField: NonSendableType // expected-note 4{{mutation of this property is only permitted within the actor}}
  var mainField: NonSendableType // expected-note 4{{mutation of this property is only permitted within the actor}}

  nonisolated func trigger() {}

  nonisolated init(v1: Void) {
    self.nonisoField = SendableType()
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}

    trigger() // expected-note 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoField  // OK - explicitly nonisolated
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{cannot access property 'customField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{cannot access property 'mainField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @MainActor init(v2: Void) {
    self.nonisoField = SendableType()
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType()

    trigger()

    _ = self.nonisoField  // OK - explicitly nonisolated
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType()
  }

  @CustomActor init(v3: Void) {
    self.nonisoField = SendableType()
    self.customField = NonSendableType()
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    trigger() // expected-note 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoField  // OK - explicitly nonisolated
    self.customField = NonSendableType() // expected-warning {{cannot access property 'customField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{cannot access property 'mainField' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }
}

//////////////////////////////////////////////////////////////////////////////////
// MARK: Mixed Default Requirements with Different Default Value Initialization //
// Isolation                                                                    //
//////////////////////////////////////////////////////////////////////////////////

// This test has fields that have an explicitly isolated initializer (e.x.:
// requiresMainActor).
@available(SwiftStdlib 5.5, *)
@MainActor
class GADTStructWithMixedDefaultRequirements {
  var mainDefault: NonSendableType = requiresMainActor() // expected-note 4{{main actor-isolated default value of 'self.mainDefault' cannot be used in a global actor 'CustomActor'-isolated initializer}}
  // expected-note @-1 6{{property declared here}}
  // expected-note @-2 3{{mutation of this property is only permitted within the actor}}
  nonisolated let nonisoNoDefault: SendableType
  @CustomActor var customDefault: NonSendableType = requiresCustomActor() // expected-note 2{{global actor 'CustomActor'-isolated default value of 'self.customDefault' cannot be used in a nonisolated initializer}}
  // expected-note @-1 2{{global actor 'CustomActor'-isolated default value of 'self.customDefault' cannot be used in a main actor-isolated initializer}}
  // expected-note @-2 2{{mutation of this property is only permitted within the actor}}
  // expected-note @-3 4{{property declared here}}

  nonisolated func trigger() {}

  // We do not completely initialize since we cannot run the default init of
  // customDefault its initializer actually requires us to be on CustomActor.
  @MainActor init(v1: NonSendableType) {
    self.nonisoNoDefault = SendableType()
    _ = self.mainDefault

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault
    _ = self.customDefault // expected-error {{variable 'self.customDefault' used before being initialized}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  // We do completely initialize. With future changes, we should be able to do
  // this.
  @MainActor init(v1a: NonSendableType) {
    self.nonisoNoDefault = SendableType()
    _ = self.mainDefault
    self.customDefault = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoNoDefault
    _ = self.mainDefault
    _ = self.customDefault // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  }

  // We do not completely initialize since we cannot run the default init of
  // mainDefault its initializer actually requires us to be on MainDefault.
  @CustomActor init(v1b: NonSendableType) {
    self.nonisoNoDefault = SendableType()
    _ = self.customDefault

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault // expected-error {{variable 'self.mainDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  // We do not completely initialize since we cannot run the default init of
  // mainDefault its initializer actually requires us to be on MainDefault.
  @CustomActor init(v1bb: NonSendableType) {
    self.nonisoNoDefault = SendableType()
    _ = self.customDefault
    _ = self.mainDefault // expected-error {{variable 'self.mainDefault' used before being initialized}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault // expected-error {{variable 'self.mainDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  // We do completely initialize. With future changes, we should be able to do
  // this.
  @CustomActor init(v1c: NonSendableType) {
    self.nonisoNoDefault = SendableType()
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault

    trigger() // expected-note 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  nonisolated init(v2: Void) {
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-error {{variable 'self.customDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  nonisolated init(v3: Void) {
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefault = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()

    trigger() // expected-note 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
  }
}

@available(SwiftStdlib 5.5, *)
@MainActor
class GADTStructWithMixedDefaultRequirementsNonIsolatedInits {
  var mainDefault = NonSendableType() // expected-note 2{{mutation of this property is only permitted within the actor}}
  // expected-note @-1 5{{property declared here}}
  nonisolated let nonisoNoDefault: SendableType
  @CustomActor var customDefault = NonSendableType() // expected-note 4{{property declared here}}
  // expected-note @-1 {{mutation of this property is only permitted within the actor}}

  nonisolated func trigger() {}

  @MainActor init(v1: Void) {
    self.nonisoNoDefault = SendableType()
    _ = self.mainDefault
    _ = self.customDefault // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoNoDefault
    _ = self.mainDefault
    _ = self.customDefault // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  }

  // OK: explicitly initializes all fields
  @CustomActor init(v1a: Void) {
    self.nonisoNoDefault = SendableType()
    _ = self.mainDefault // expected-warning {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault

    trigger() // expected-note 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init(v1b: Void) {
    self.nonisoNoDefault = SendableType()
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.customDefault = NonSendableType()

    trigger() // expected-note 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  nonisolated init(v2: Void) {
    self.nonisoNoDefault = SendableType()

    trigger() // expected-note 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
  }

  nonisolated init(v2a: Void) {
    self.nonisoNoDefault = SendableType()
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefault = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}

    trigger() // expected-note 2{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
  }
}

////////////////////////////////////////////////////
// MARK: Address Only Struct Variants             //
////////////////////////////////////////////////////

@available(SwiftStdlib 5.1, *)
@MainActor
struct MainActorStructWithMixedFields_AO<T> {
  var _addressOnly: T // expected-note 4{{mutation of this property is only permitted within the actor}}
  nonisolated let nonisoField: SendableType
  nonisolated let nonisoSendableField: SendableType
  @CustomActor var customField: NonSendableType // expected-note 8{{mutation of this property is only permitted within the actor}}
  @CustomActor var customFieldAO: NonSendableAOStruct<T> // expected-note 8{{mutation of this property is only permitted within the actor}}
  var mainField: NonSendableType // expected-note 8{{mutation of this property is only permitted within the actor}}
  var mainFieldAO: NonSendableAOStruct<T> // expected-note 8{{mutation of this property is only permitted within the actor}}

  nonisolated func trigger() {}

  nonisolated init(v1: Void, t: sending T, t2: sending T, t3: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoField = SendableType()
    self.nonisoSendableField = SendableType()
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customFieldAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customFieldAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainFieldAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainFieldAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoField
    _ = self.nonisoSendableField
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customFieldAO = NonSendableAOStruct(value: t3) // expected-warning {{global actor 'CustomActor'-isolated property 'customFieldAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainFieldAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainFieldAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
  }

  nonisolated init?(v1_failable: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoField = SendableType()
    self.nonisoSendableField = SendableType()
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customFieldAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customFieldAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainFieldAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainFieldAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoField
    _ = self.nonisoSendableField
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customFieldAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customFieldAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainFieldAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainFieldAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
  }

  @MainActor init(v2: Void, t: sending T, t2: sending T) {
    self._addressOnly = t
    self.nonisoField = SendableType()
    self.nonisoSendableField = SendableType()
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.customFieldAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customFieldAO' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType()
    self.mainFieldAO = NonSendableAOStruct(value: t)

    trigger()

    _ = self.nonisoField
    _ = self.nonisoSendableField
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.customFieldAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customFieldAO' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType()
    self.mainFieldAO = NonSendableAOStruct(value: t)
  }

  @MainActor init?(v2_failable: Void, t: sending T, t2: sending T) {
    self._addressOnly = t
    self.nonisoField = SendableType()
    self.nonisoSendableField = SendableType()
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.customFieldAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customFieldAO' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType()
    self.mainFieldAO = NonSendableAOStruct(value: t)

    trigger()

    _ = self.nonisoField
    _ = self.nonisoSendableField
    self.customField = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customField' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.customFieldAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customFieldAO' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.mainField = NonSendableType()
    self.mainFieldAO = NonSendableAOStruct(value: t)
  }

  @CustomActor init(v3: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.nonisoField = SendableType()
    self.nonisoSendableField = SendableType()
    self.customField = NonSendableType()
    self.customFieldAO = NonSendableAOStruct(value: t2)
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainFieldAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainFieldAO' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoField
    _ = self.nonisoSendableField
    self.customField = NonSendableType()
    self.customFieldAO = NonSendableAOStruct(value: t2)
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainFieldAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainFieldAO' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init?(v3_failable: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.nonisoField = SendableType()
    self.nonisoSendableField = SendableType()
    self.customField = NonSendableType()
    self.customFieldAO = NonSendableAOStruct(value: t2)
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainFieldAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainFieldAO' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoField
    _ = self.nonisoSendableField
    self.customField = NonSendableType()
    self.customFieldAO = NonSendableAOStruct(value: t2)
    self.mainField = NonSendableType() // expected-warning {{main actor-isolated property 'mainField' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainFieldAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainFieldAO' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
  }
}

@available(SwiftStdlib 5.5, *)
@MainActor
class GADTStructWithMixedDefaultRequirements_AO<T> {
  var _addressOnly: T // expected-note 7{{mutation of this property is only permitted within the actor}}
  var mainDefault: NonSendableType = requiresMainActor() // expected-note 4{{main actor-isolated default value of 'self.mainDefault' cannot be used in a global actor 'CustomActor'-isolated initializer}}
  // expected-note @-1 8{{property declared here}}
  // expected-note @-2 5{{mutation of this property is only permitted within the actor}}
  var mainDefaultAO: NonSendableAOStruct<T> // expected-note 5{{mutation of this property is only permitted within the actor}}
  // expected-note @-1 8{{property declared here}}
  // expected-note @-2 4{{'self.mainDefaultAO' not initialized}}
  nonisolated let nonisoNoDefault: SendableType
  nonisolated let nonisoSendableField: SendableType
  @CustomActor var customDefault: NonSendableType = requiresCustomActor() // expected-note 3{{global actor 'CustomActor'-isolated default value of 'self.customDefault' cannot be used in a main actor-isolated initializer}}
  // expected-note @-1 7{{property declared here}}
  // expected-note @-2 4{{mutation of this property is only permitted within the actor}}
  // expected-note @-3 2{{global actor 'CustomActor'-isolated default value of 'self.customDefault' cannot be used in a nonisolated initializer}}

  @CustomActor var customDefaultAO: NonSendableAOStruct<T> // expected-note 7{{property declared here}}
  // expected-note @-1 5{{'self.customDefaultAO' not initialized}}
  // expected-note @-2 4{{mutation of this property is only permitted within the actor}}

  nonisolated func trigger() {}

  // We do not completely initialize since we cannot run the default init of
  // customDefault its initializer actually requires us to be on CustomActor.
  @MainActor init(v1: NonSendableType, t: sending T) {
    self._addressOnly = t
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefaultAO = NonSendableAOStruct(value: t)
    _ = self.mainDefault

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}


    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault
    _ = self.mainDefaultAO
    _ = self.customDefault // expected-error {{variable 'self.customDefault' used before being initialized}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}

    _ = self.customDefaultAO // expected-error {{variable 'self.customDefaultAO' used before being initialized}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}

  } // expected-error {{return from initializer without initializing all stored properties}}

  @MainActor init?(v1_failable: NonSendableType, t: sending T) {
    self._addressOnly = t
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefaultAO = NonSendableAOStruct(value: t)
    _ = self.mainDefault

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}


    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault
    _ = self.mainDefaultAO
    _ = self.customDefault // expected-error {{variable 'self.customDefault' used before being initialized}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}

    _ = self.customDefaultAO // expected-error {{variable 'self.customDefaultAO' used before being initialized}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}

  } // expected-error {{variable 'self.customDefault' used before being initialized}}

  // We do completely initialize. With future changes, we should be able to do
  // this.
  @MainActor init(v1a: NonSendableType, t: sending T, t2: sending T) {
    self._addressOnly = t
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefaultAO = NonSendableAOStruct(value: t)
    _ = self.mainDefault
    self.customDefault = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault
    _ = self.mainDefaultAO
    _ = self.customDefault // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  }

  @MainActor init?(v1a_failable: NonSendableType, t: sending T, t2: sending T) {
    self._addressOnly = t
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefaultAO = NonSendableAOStruct(value: t)
    _ = self.mainDefault
    self.customDefault = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault
    _ = self.mainDefaultAO
    _ = self.customDefault // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  }

  // We do not completely initialize since we cannot run the default init of
  // mainDefault its initializer actually requires us to be on MainDefault.
  @CustomActor init(v1b: NonSendableType, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.customDefaultAO = NonSendableAOStruct(value: t2)
    _ = self.customDefault

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}


    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-error {{variable 'self.mainDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    _ = self.mainDefaultAO // expected-error {{variable 'self.mainDefaultAO' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{main actor-isolated property 'mainDefaultAO' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  // We do not completely initialize since we cannot run the default init of
  // mainDefault its initializer actually requires us to be on MainDefault.
  @CustomActor init(v1bb: NonSendableType, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.customDefaultAO = NonSendableAOStruct(value: t2)
    _ = self.customDefault
    _ = self.mainDefault // expected-error {{variable 'self.mainDefault' used before being initialized}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-error {{variable 'self.mainDefaultAO' used before being initialized}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}


    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-error {{variable 'self.mainDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    _ = self.mainDefaultAO // expected-error {{variable 'self.mainDefaultAO' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{main actor-isolated property 'mainDefaultAO' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}

    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  // We do completely initialize. With future changes, we should be able to do
  // this.
  @CustomActor init(v1c: NonSendableType, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2)
    _ = self.customDefault

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init?(v1c_failable: NonSendableType, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2)
    _ = self.customDefault

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  nonisolated init(v2: Void, t: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()

    trigger() // expected-error {{'self' used in method call 'trigger' before all stored properties are initialized}}
    // expected-note @-1 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}


    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-error {{variable 'self.customDefault' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}

    _ = self.customDefaultAO // expected-error {{variable 'self.customDefaultAO' used before being initialized}}
    // expected-warning @-1 {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-2 {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}

  } // expected-error {{return from initializer without initializing all stored properties}}

  nonisolated init(v3: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefault = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
  }

  nonisolated init?(v3_failable: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefault = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
  }
}

@available(SwiftStdlib 5.5, *)
@MainActor
class GADTStructWithMixedDefaultRequirementsNonIsolatedInits_AO<T> {
  var _addressOnly: T // expected-note 6{{mutation of this property is only permitted within the actor}}
  var mainDefault = NonSendableType() // expected-note 3{{mutation of this property is only permitted within the actor}}
  // expected-note @-1 7{{property declared here}}
  var mainDefaultAO: NonSendableAOStruct<T> // expected-note 6{{property declared here}}
  // expected-note @-1 6{{mutation of this property is only permitted within the actor}}
  nonisolated let nonisoNoDefault: SendableType
  nonisolated let nonisoSendableField: SendableType
  @CustomActor var customDefault = NonSendableType() // expected-note 7{{property declared here}}
  // expected-note @-1 {{mutation of this property is only permitted within the actor}}
  @CustomActor var customDefaultAO: NonSendableAOStruct<T> // expected-note 5{{property declared here}}
  // expected-note @-1 5{{mutation of this property is only permitted within the actor}}

  nonisolated func trigger() {}

  @MainActor init(v1: Void, t: sending T, t2: sending T) {
    self._addressOnly = t
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefaultAO = NonSendableAOStruct(value: t)
    self.customDefaultAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    _ = self.mainDefault
    _ = self.customDefault // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault
    _ = self.mainDefaultAO
    _ = self.customDefault // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  }

  @MainActor init?(v1_failable: Void, t: sending T, t2: sending T) {
    self._addressOnly = t
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefaultAO = NonSendableAOStruct(value: t)
    self.customDefaultAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be mutated from the main actor; this is an error in the Swift 6 language mode}}
    _ = self.mainDefault
    _ = self.customDefault // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}

    trigger()

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault
    _ = self.mainDefaultAO
    _ = self.customDefault // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from the main actor; this is an error in the Swift 6 language mode}}
  }

  // OK: explicitly initializes all fields
  @CustomActor init(v1a: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2)
    _ = self.mainDefault // expected-warning {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init(v1b: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.customDefault = NonSendableType()
    self.customDefaultAO = NonSendableAOStruct(value: t2)

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  @CustomActor init?(v1b_failable: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    self.customDefault = NonSendableType()
    self.customDefaultAO = NonSendableAOStruct(value: t2)

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from global actor 'CustomActor'; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
  }

  nonisolated init(v2: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
  }

  nonisolated init?(v2_failable: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
  }

  nonisolated init(v2a: Void, t: sending T, t2: sending T) {
    self._addressOnly = t // expected-warning {{main actor-isolated property '_addressOnly' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.nonisoNoDefault = SendableType()
    self.nonisoSendableField = SendableType()
    self.mainDefault = NonSendableType() // expected-warning {{main actor-isolated property 'mainDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.mainDefaultAO = NonSendableAOStruct(value: t) // expected-warning {{main actor-isolated property 'mainDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefault = NonSendableType() // expected-warning {{global actor 'CustomActor'-isolated property 'customDefault' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.customDefaultAO = NonSendableAOStruct(value: t2) // expected-warning {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}

    trigger() // expected-note 4{{after calling instance method 'trigger()', only nonisolated properties of 'self' can be accessed from this init}}

    _ = self.nonisoNoDefault
    _ = self.nonisoSendableField
    _ = self.mainDefault // expected-warning {{cannot access property 'mainDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.mainDefaultAO // expected-warning {{cannot access property 'mainDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{main actor-isolated property 'mainDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefault // expected-warning {{cannot access property 'customDefault' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefault' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
    _ = self.customDefaultAO // expected-warning {{cannot access property 'customDefaultAO' here in nonisolated initializer; this is an error in the Swift 6 language mode; this is an error in the Swift 6 language mode}}
    // expected-warning @-1 {{global actor 'CustomActor'-isolated property 'customDefaultAO' can not be referenced from a nonisolated context; this is an error in the Swift 6 language mode}}
  }
}
