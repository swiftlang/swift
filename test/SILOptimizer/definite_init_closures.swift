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

struct InoutUse {
  var x: Bool
  var y: Bool

  init() {
    x = false
    y = false || forward(&x)
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

