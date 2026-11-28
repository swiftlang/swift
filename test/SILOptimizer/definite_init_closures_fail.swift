// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify

// Test boolean operators with implicit closures

struct Simple {
  let x: Bool // expected-note {{'self.x' not initialized}}
              // expected-note @-1 {{'self.x' not initialized}}
  let y: Bool

  init() {
    y = false || x // expected-error {{constant 'self.x' used before being initialized}}
  } // expected-error {{return from initializer without initializing all stored properties}}

  init(b: Bool) {
    if b {
      x = false
    }
    y = false || x // expected-error {{constant 'self.x' used before being initialized}}
  } // expected-error {{return from initializer without initializing all stored properties}}
}

struct NestedClosures {
  let x: Bool // expected-note {{'self.x' not initialized}}
  let y: Bool
  let z: Bool

  init(_ a: Bool) {
    y = false
    z = false || (y || (x || a)) // expected-error {{constant 'self.x' used before being initialized}}
  } // expected-error {{return from initializer without initializing all stored properties}}
}

class SimpleClass {
  let x: Bool // expected-note {{'self.x' not initialized}}
  let y: Bool

  init() {
    y = false || x // expected-error {{constant 'self.x' used before being initialized}}
  } // expected-error {{return from initializer without initializing all stored properties}}
}

func forward(_ b: inout Bool) -> Bool {
  return b
}

struct InoutUse {
  var x: Bool
  var y: Bool

  init() {
    y = false || forward(&x) // expected-error {{variable 'self.x' passed by reference before being initialized}}
  }
}

protocol P {
  var b: Bool { get }
}

struct Generic<T : P> {
  let x: T // expected-note {{'self.x' not initialized}}
  let y: Bool

  init(_ t: T) {
    y = false || x.b // expected-error {{constant 'self.x' used before being initialized}}
  } // expected-error {{return from initializer without initializing all stored properties}}
}

func captureUninitialized() {
  let fn: () -> Void // expected-note {{constant defined here}}
  fn = { fn() } // expected-error {{constant 'fn' captured by a closure before being initialized}}
}

// rdar://103800778
struct StructCaptureSelfBeforeAllMembersInit {
  let count = 10
  let points: [(Double, Double)] // expected-note {{'self.points' not initialized}}

  init() {
    let linear = (0...count).map { (i: Int) in Double(i) / Double(count) } // expected-error {{'self' captured by a closure before all members were initialized}}
    points = linear.map { ($0, $0) }
  }
}

struct StructEscapingClosureMultipleUninit {
  let initialized = 1
  var uninitVar: Int // expected-note {{'self.uninitVar' not initialized}}
  let uninitLet: String // expected-note {{'self.uninitLet' not initialized}}

  init() {
    let _: () -> Int = { initialized } // expected-error {{'self' captured by a closure before all members were initialized}}
    uninitVar = 2
    uninitLet = "hi"
  }
}

struct GenericStructCaptureSelf<T> {
  let count = 10
  let value: T // expected-note {{'self.value' not initialized}}

  init(_ v: T) {
    let _ = (0...count).map { i in i + count } // expected-error {{'self' captured by a closure before all members were initialized}}
    value = v
  }
}

struct AutoclosureExistentialMetatype {
  var a = 0
  let b: any P

  init(_ p: any P) {
    _ = false || (type(of: b) == Int.self) // expected-error {{constant 'self.b' used before being initialized}}
    b = p
  }
}

struct AutoclosureForceUnwrapGeneric<T: Equatable> {
  let b: T?

  init(_ v: T?) {
    _ = false || (b! == b!) // expected-error {{constant 'self.b' used before being initialized}}
    b = v
  }
}

func existentialMetatypeUninit() {
  let x: any P // expected-note {{constant defined here}}
  _ = type(of: x) // expected-error {{constant 'x' used before being initialized}}
}
