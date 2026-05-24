// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify

struct EmptyStruct {}

struct ValueStruct {
  var ivar: EmptyStruct // expected-note {{'self.ivar' not initialized}}

  init() { ivar = EmptyStruct() }

  init(a: Int) {
    _ = ivar // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.init()
  }

  init(c: Bool) {
    if c {
      return
    }

    self.init()
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(d: Bool) {
    if d {
      return // expected-error {{return from initializer without initializing all stored properties}}
    }

    self = ValueStruct()
  }
}

enum ValueEnum {
  case Dinosaur, Train, Truck

  init() { self = .Train }

  init(a: Int) {
    _ = self // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.init()
  }

  init(c: Bool) {
    if c {
      return
    }

    self.init()
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(d: Bool) {
    if d {
      return
    }

    self = ValueEnum()
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}

struct AddressStruct {
  var ivar: EmptyStruct // expected-note {{'self.ivar' not initialized}}
  var any: Any?

  init() { ivar = EmptyStruct(); any = nil }

  init(c: Bool) {
    if c {
      return
    }

    self.init()
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(d: Bool) {
    if d {
      return
    }

    self = AddressStruct()
  } // expected-error {{return from initializer without initializing all stored properties}}
}

// Old versions of swift-syntax have this logical use-before-definition; make
// sure we keep it working.
struct InitLogicalUseBeforeDef {
  let offset: UInt16

  init?(value: Int) {
    if value > type(of: self.offset).max {
      return nil
    }
    offset = UInt16(value)
  }
}

// Cases motivated by:
// https://forums.swift.org/t/value-semantics-of-init-with-defaulted-private-let-fields-does-not-make-sense/86193/
// Full self assignment of structs within their initializers should not trigger
// errors if `let` fields have already been initialized.

struct PaddedLet {
  let x: Int
  private let padding: Int = 0
  init(x: Int) { self.x = x }
}

extension PaddedLet {
  init() {
    // `padding` is default-initialized here, but a full self-assign should
    // not complain about that.
    self = .init(x: 0)
  }

  // Multiple whole-self overwrites are allowed.
  init(twice: Bool) {
    self = .init(x: 0)
    self = .init(x: 1)
  }
}

struct AllLet {
  let x: Int // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  let y: Int

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }
}

extension AllLet {
  // Whole-self overwrite after element-level init is permitted.
  init(rebuild xx: Int) {
    self.x = xx
    self.y = 0
    self = .init(x: 0, y: 0)
  }

  // Partial init followed by whole-self overwrite is allowed.
  init(partialInit z: Int) {
    self.x = z
    self = .init(x: 1, y: 2)
  }

  // Element-level overwrite of a `let` after `self = ...` is still rejected.
  init(other: AllLet, x: Int) {
    self = other
    self.x = x // expected-error {{immutable value 'self.x' may only be initialized once}}
  }
}

struct NestedLet {
  let tup: (Int, (Bool, String?)) // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  var mut = 0
}

extension NestedLet {
  init(zzz: Void) {
    self.tup.1.0 = true
    self = .init(tup: (1, (true, nil)), mut: 1)
    self.tup.1.0 = false // expected-error {{immutable value 'self.tup.1.0' may only be initialized once}}
  }
}

// Composes with move-only checking.

struct Resource: ~Copyable {
  let x: Int
  var mut: Int = 0
  private let padding: Int = 0
  init(x: Int) { self.x = x }
}

extension Resource {
  init() {
    self = .init(x: 0)
  }

  init(z: Int) {
    self.x = z
    self = .init(x: z)
  }

  init(multiInit x: consuming Resource, y: consuming Resource) {
    self.mut = 1
    self = x
    self.mut = 2
    self = y
    self.mut = 3
  }
}

// Failable and throwing initializers should have analogous diagnostic behavior

struct Fallible {
  let x: Int
  private let padding: Int = 0
  init(x: Int) { self.x = x }
}

extension Fallible {
  init?(maybe x: Int?) {
    guard let x else { return nil }
    self = .init(x: x)
  }

  init?(cond: Bool, x: Int) {
    if cond {
      self = .init(x: x)
    } else {
      return nil
    }
  }
}

struct Throwing {
  let x: Int
  private let padding: Int = 0
  init(x: Int) { self.x = x }

  static func makeOrThrow(x: Int) throws -> Throwing {
    return .init(x: x)
  }
}

extension Throwing {
  init(rethrowing x: Int) throws {
    self = try Self.makeOrThrow(x: x)
  }

  init(rethrowingTwice x: Int) throws {
    self = try Self.makeOrThrow(x: x)
    self = try Self.makeOrThrow(x: x + 1)
  }
}
