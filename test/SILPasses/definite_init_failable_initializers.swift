// RUN: %target-swift-frontend -emit-sil -disable-objc-attr-requires-foundation-module -verify %s

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

func unwrap(i: Int) throws -> Int {
  return i
}

// For classes, we cannot yet support failure with a partially initialized
// object.
// TODO: We ought to be able to for native Swift classes.

class RootClass {
  let x: Int  // expected-note {{'self.x' not initialized}}
  let y: Int  // expected-note 2 {{'self.y' not initialized}}

  init() { x = 0; y = 0 }

  convenience init?(failBeforeDelegation: Bool) {
    if failBeforeDelegation { return nil } // ok
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
    if failBeforeFailableDelegation { return nil }     // ok

    self.init(failBeforeInitialization: ())
  }

  convenience init?(failAfterFailableDelegation: ()) {
    self.init(failBeforeInitialization: ())
    return nil // OK
  }

  init(throwBeforeInitialization: Int) throws {
    self.x = throwBeforeInitialization
    self.y = throwBeforeInitialization
  }
}

class SubClass: RootClass {
  let z: Int   // expected-note {{'self.z' not initialized}}

  override init() {
    z = 0
    super.init()
  }

  override init?(failBeforeInitialization: ()) {
    // expected-error@+2{{properties of a class instance must be initialized before returning nil}}
    // expected-note@+1{{super.init must be called before returning nil}}
    return nil
  }

  init?(failBeforeSuperInitialization: ()) {
    z = 0
    // expected-error@+2{{properties of a class instance must be initialized before returning nil}}
    // expected-note@+1{{super.init must be called before returning nil}}
    return nil
  }

  override init?(failAfterFullInitialization: ()) {
    z = 0
    super.init()
    return nil // OK
  }

  init?(failBeforeFailableSuperInit: Bool) {
    z = 0
    if failBeforeFailableSuperInit { return nil }      // expected-error{{properties of a class instance must be initialized before returning nil}}
    // expected-note@-1{{super.init must be called before returning nil}}

    super.init(failBeforeInitialization: ())
  }

  init?(failAfterFailableSuperInit: Bool) {
    z = 0
    super.init(failBeforeInitialization: ())
    return nil // OK
  }

  convenience init?(failBeforeDelegation: Bool) {
    if failBeforeDelegation { return nil } // ok
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

class AnotherSubClass: RootClass {
  override init(throwBeforeInitialization: Int) throws {
    // FIXME: bogus diagnosics about returning nil here
    try super.init(throwBeforeInitialization: unwrap(throwBeforeInitialization))
    // expected-error@-1 {{all stored properties of a class instance must be initialized before throwing from an initializer}}
    // expected-error@-2 {{all stored properties of a class instance must be initialized before returning nil from an initializer}}
    // expected-note@-3 {{super.init must be called before throwing}}
    // expected-note@-4 {{super.init must be called before returning nil}}

    try unwrap(throwBeforeInitialization)
  }
}

// <rdar://problem/21087069> Unexpected Error calling delegating initializer from delegating initializer marked with throws
struct ThrowStruct {
  var x : String
  init() throws { x = "" }
  
  init(a : Int) throws {
    try self.init()
  }
}

enum Err : ErrorType { case X }

struct ThrowAddrOnlyStruct<T> {
  var x : T
  init() throws { throw Err.X }
  
  init(a : Int) throws {
    try self.init()
  }
}

// Delegation to other failable initializers
struct S1 {
  init?(a: Int64) { return nil }

  init!(b: Int64) {
    self.init(a: b)! // unnecessary-but-correct '!'
  }

  init(c: Int64) {
    self.init(a: c)! // necessary '!'
  }

  init(d: Int64) {
    self.init(b: d)! // unnecessary-but-correct '!'
  }
}

enum E1 {
  case A

  init?(a: Int64) { self = .A }

  init!(b: Int64) {
    self.init(a: b)! // unnecessary-but-correct '!'
  }

  init(c: Int64) {
    self.init(a: c)! // necessary '!'
  }

  init(d: Int64) {
    self.init(b: d)! // unnecessary-but-correct '!'
  }
}

class C1 {
  var member: Int64

  init?(a: Int64) {
    self.member = a
  }

  convenience init!(b: Int64) {
    self.init(a: b)! // unnecessary-but-correct '!'
  }

  convenience init(c: Int64) {
    self.init(a: c)! // necessary '!'
  }

  convenience init(d: Int64) {
    self.init(b: d)! // unnecessary-but-correct '!'
  }
}

// Chaining to failable initializers in a superclass
class C2 : C1 {
  var otherMember: Int64

  init(e: Int64) {
    self.otherMember = e
    super.init(a: e)! // necessary '!'
  }

  init!(f: Int64) {
    self.otherMember = f
    super.init(a: f)! // unnecessary-but-correct '!'
  }
}

// Delegating to failable initializers from a protocol extension to a
// protocol.
protocol P1 {
  init?(p1: Int64)
}

extension P1 {
  init!(p1a: Int64) {
    self.init(p1: p1a)! // unnecessary-but-correct '!'
  }

  init(p1b: Int64) {
    self.init(p1: p1b)! // necessary '!'
  }
}

protocol P2 : class {
  init?(p2: Int64)
}

extension P2 {
  init!(p2a: Int64) {
    self.init(p2: p2a)! // unnecessary-but-correct '!'
  }

  init(p2b: Int64) {
    self.init(p2: p2b)! // necessary '!'
  }
}

@objc protocol P3 {
  init?(p3: Int64)
}

extension P3 {
  init!(p3a: Int64) {
    self.init(p3: p3a)! // unnecessary-but-correct '!'
  }

  init(p3b: Int64) {
    self.init(p3: p3b)! // necessary '!'
  }
}

// Delegating to failable initializers from a protocol extension to a
// protocol extension.
extension P1 {
  init?(p1c: Int64) {
    self.init(p1: p1c)
  }

  init!(p1d: Int64) {
    self.init(p1c: p1d)! // unnecessary-but-correct '!'
  }

  init(p1e: Int64) {
    self.init(p1c: p1e)! // necessary '!'
  }
}

extension P2 {
  init?(p2c: Int64) {
    self.init(p2: p2c)
  }

  init!(p2d: Int64) {
    self.init(p2c: p2d)! // unnecessary-but-correct '!'
  }

  init(p2e: Int64) {
    self.init(p2c: p2e)! // necessary '!'
  }
}
