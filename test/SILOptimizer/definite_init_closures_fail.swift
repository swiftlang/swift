// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify

// Test boolean operators with implicit closures

struct Simple {
  let x: Bool // expected-note {{'self.x' not initialized}}
              // expected-note @-1 {{'self.x' not initialized}}
  let y: Bool

  init() { // expected-error {{return from initializer without initializing all stored properties}} {{12:3-3=self.x = x\n}} {{8-8=x: Bool}}
    y = false || x // expected-error {{constant 'self.x' used before being initialized}}
  }

  init(b: Bool) { // expected-error {{return from initializer without initializing all stored properties}} {{19:3-3=self.x = x\n}} {{15-15=, x: Bool}}
    if b {
      x = false
    }
    y = false || x // expected-error {{constant 'self.x' used before being initialized}}
  } 
}

struct NestedClosures {
  let x: Bool // expected-note {{'self.x' not initialized}}
  let y: Bool
  let z: Bool

  init(_ a: Bool) {// expected-error {{return from initializer without initializing all stored properties}} {{30:3-3=self.x = x\n}} {{17-17=, x: Bool}}
    y = false
    z = false || (y || (x || a)) // expected-error {{constant 'self.x' used before being initialized}}
  }
}

class SimpleClass {
  let x: Bool // expected-note {{'self.x' not initialized}}
  let y: Bool

  init() {// expected-error {{return from initializer without initializing all stored properties}} {{39:3-3=self.x = x\n}} {{8-8=x: Bool}}
    y = false || x // expected-error {{constant 'self.x' used before being initialized}}
  }
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

  init(_ t: T) {// expected-error {{return from initializer without initializing all stored properties}} {{65:3-3=self.x = x\n}} {{14-14=, x: T}}
    y = false || x.b // expected-error {{constant 'self.x' used before being initialized}}
  }
}

