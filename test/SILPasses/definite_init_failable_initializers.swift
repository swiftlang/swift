// High-level tests that DI accepts and rejects failure from failable
// initializers properly.

// RUN: %swift -emit-sil -verify %s

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

// For classes, we only support failure after the object has been fully
// initialized. Although we could support failure with partial initialization
// for fully native Swift classes, it would be difficult to support for ObjC
// and mixed-heritage classes because of Cocoa's monolithic dealloc interface.

class RootClass {
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
    return nil // OK
  }

  convenience init?(failBeforeDelegation: Bool) {
    if failBeforeDelegation { return nil }
    self.init()
  }

  convenience init?(failAfterDelegation: ()) {
    self.init()
    return nil // OK
  }

  convenience init?(failBeforeFailableDelegation: Bool) {
    if failBeforeFailableDelegation { return nil }
    self.init(failBeforeInitialization: ())
  }

  convenience init?(failAfterFailableDelegation: ()) {
    self.init(failBeforeInitialization: ())
    return nil // OK
  }
}

class SubClass: RootClass {
  let z: Int

  override init() {
    z = 0
    super.init()
  }

  override init?(failBeforeInitialization: ()) {
    return nil
  }

  init?(failBeforeSuperInitialization: ()) {
    z = 0
    return nil
  }

  override init?(failAfterFullInitialization: ()) {
    z = 0
    super.init()
    return nil // OK
  }

  init?(failBeforeFailableSuperInit: Bool) {
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
    if failBeforeDelegation { return nil }
    self.init()
  }

  convenience init?(failAfterDelegation: ()) {
    self.init()
    return nil // OK
  }

  convenience init?(failBeforeFailableDelegation: Bool) {
    if failBeforeFailableDelegation { return nil }
    self.init(failBeforeInitialization: ())
  }

  convenience init?(failAfterFailableDelegation: ()) {
    self.init(failBeforeInitialization: ())
    return nil // OK
  }
}
