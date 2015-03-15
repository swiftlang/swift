// RUN: %target-swift-frontend -emit-sil -verify %s

// High-level tests that DI accepts and rejects failure from failable
// initializers properly.

// For value types, we can handle failure at any point, using DI's established
// analysis for partial struct and tuple values.

struct Struct {
  let x, y: Int 

  init() { x = 0; y = 0 }

  init?(failBeforeInitialization: ()) {
    return nil
  }

  init?(failAfterPartialInitialization: ()) {
    x = 0
    return nil
  }

  init?(failAfterFullInitialization: ()) {
    x = 0
    y = 0
    return nil
  }

  init?(failAfterWholeObjectInitializationByAssignment: ()) {
    self = Struct()
    return nil
  }

  init?(failAfterWholeObjectInitializationByDelegation: ()) {
    self.init()
    return nil
  }
}

// For classes, we cannot yet support failure with a partially initialized
// object.
// TODO: We ought to be able to for native Swift classes.

class RootClass {
  let x : Int  // expected-note {{'self.x' not initialized}}
  let y: Int  // expected-note 2 {{'self.y' not initialized}}

  init() { x = 0; y = 0 }

  convenience init?(failBeforeDelegation: Bool) {  // expected-error{{failable convenience initializer must delegate to self.init() before returning nil}}
    if failBeforeDelegation { return nil }
    self.init()
  }

  convenience init?(failAfterDelegation: ()) {
    self.init()
    return nil // OK
  }

  init?(failBeforeInitialization: ()) {
    return nil // expected-error{{properties of a class instance must be initialized before returning nil}}
  }

  init?(failAfterPartialInitialization: ()) {
    x = 0
    return nil // expected-error{{properties of a class instance must be initialized before returning nil}}
  }

  init?(failAfterFullInitialization: ()) {
    x = 0
    y = 0
    return nil // OK
  }

  convenience init?(failBeforeFailableDelegation: Bool) {
    // expected-error@-1{{failable convenience initializer must delegate to self.init() before returning nil}}
    if failBeforeFailableDelegation { return nil }
    self.init(failBeforeInitialization: ())
  }

  convenience init?(failAfterFailableDelegation: ()) {
    self.init(failBeforeInitialization: ())
    return nil // OK
  }
}

class SubClass: RootClass {
  let z: Int   // expected-note {{'self.z' not initialized}}

  override init() {
    z = 0
    super.init()
  }

  override init?(failBeforeInitialization: ()) {
    // expected-error@-1{{properties of a class instance must be initialized before returning nil}}
    // expected-note@-2{{super.init must be called before returning nil}}
    return nil
  }

  init?(failBeforeSuperInitialization: ()) {
    // expected-error@-1{{properties of a class instance must be initialized before returning nil}}
    // expected-note@-2{{super.init must be called before returning nil}}
    z = 0
    return nil
  }

  override init?(failAfterFullInitialization: ()) {
    z = 0
    super.init()
    return nil // OK
  }

  init?(failBeforeFailableSuperInit: Bool) {
    // expected-error@-1{{properties of a class instance must be initialized before returning nil}}
    // expected-note@-2{{super.init must be called before returning nil}}
    z = 0
    if failBeforeFailableSuperInit { return nil }
    super.init(failBeforeInitialization: ())
  }

  init?(failAfterFailableSuperInit: Bool) {
    z = 0
    super.init(failBeforeInitialization: ())
    return nil // OK
  }

  convenience init?(failBeforeDelegation: Bool) {
    // expected-error@-1{{failable convenience initializer must delegate to self.init() before returning nil}}
    if failBeforeDelegation { return nil }
    self.init()
  }

  convenience init?(failAfterDelegation: ()) {
    self.init()
    return nil // OK
  }

  convenience init?(failBeforeFailableDelegation: Bool) {
    // expected-error@-1{{failable convenience initializer must delegate to self.init() before returning nil}}
    if failBeforeFailableDelegation { return nil }
    self.init(failBeforeInitialization: ())
  }

  convenience init?(failAfterFailableDelegation: ()) {
    self.init(failBeforeInitialization: ())
    return nil // OK
  }
}
