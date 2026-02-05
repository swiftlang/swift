// RUN: %target-swift-frontend -emit-sil %s -o /dev/null

// Test boolean operators with implicit closures

struct Simple {
  let x: Bool
  let y: Bool

  init() {
    x = false
    y = false || x
  }
}

struct NestedClosures {
  let x: Bool
  let y: Bool
  let z: Bool

  init(a: Bool) {
    x = false
    y = false
    z = false || (y || (x || a))
  }

  init(b: Bool) {
    x = false
    y = false
    // With explicit self
    z = false || (self.y || (self.x || b))
  }
}

class SimpleClass {
  let x: Bool
  let y: Bool

  init() {
    x = false
    y = false || x
  }
}

func forward(_ b: inout Bool) -> Bool {
  return b
}

struct T {
  var i: Int

  mutating func foo() -> Bool {
    i == 0
  }
}


struct InoutUse {
  var x: Bool
  var y: Bool

  init() {
    x = false
    y = false || forward(&x)
  }
}

struct InoutSelfUse {
  var a: T
  var b: T

  init(_ x: T) {
    a = x
    if a.foo() || a.foo() {
    }
    b = x
  }
}

protocol P {
  var b: Bool { get }
}

struct Generic<T : P> {
  let x: T
  let y: Bool

  init(_ t: T) {
    x = t
    y = false || x.b
  }
}

class Base { }

class Derived : Base {
  var x: Bool
  var y: Bool

  init(_: Int) {
    x = false
    y = true || x
  }
}

