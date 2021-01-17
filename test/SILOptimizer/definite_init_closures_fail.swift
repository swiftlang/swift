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

