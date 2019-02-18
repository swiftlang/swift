// RUN: %target-typecheck-verify-swift

// Test various orderings of constructor calls and assignments to make
// sure we figure out the constructor kind in various situations.

struct Measurement {
  let val: Int
}

class Superclass {
  init() {}
  init(name: String) {}
}

class SomeClass : Superclass {
  let width: Measurement // expected-note * {{declared here}}
  let height: Measurement // expected-note * {{declared here}}

  // super.init() call gives us a chaining initializer, where we can
  // assign let properties
  override init() {
    self.width = Measurement.self.init(val: 10)
    self.height = Measurement.self.init(val: 20)
    super.init(name: "shape")
  }

  // Another case
  init(width: Int, height: Int) {
    super.init(name: "shape")
    self.width = Measurement.self.init(val: width)
    self.height = Measurement.self.init(val: height)
  }

  // Delegating initializer -- let properties are immutable
  convenience init(width: Int) {
    self.init(width: width, height: 20)
    self.height = Measurement(val: 20) // expected-error {{'let' property 'height' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
  }

  // Another case
  convenience init(height: Int) {
    self.width = Measurement(val: 20) // expected-error {{'let' property 'width' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
    self.init(width: 10, height: height)
  }
}

struct SomeStruct {
  let width: Measurement // expected-note * {{declared here}}
  let height: Measurement // expected-note * {{declared here}}

  // Delegating initializer
  init() {
    self.init()
    self.width = Measurement.self.init(val: width) // expected-error {{cannot convert value of type 'Measurement' to expected argument type 'Int'}}
    self.height = Measurement.self.init(val: height) // expected-error {{cannot convert value of type 'Measurement' to expected argument type 'Int'}}
  }

  // Designated initializer
  init(width: Int, height: Int, meta: Measurement.Type) {
    self.width = meta.init(val: width)
    self.height = meta.init(val: height)
  }

  // Delegating initializer
  init(width: Int) {
    self.init()
    self.width = width // expected-error {{cannot assign value of type 'Int' to type 'Measurement'}}
    self.height = Measurement(val: 20) // expected-error {{'let' property 'height' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
  }

  // Delegating initializer
  init(height: Int) {
    self.width = Measurement(val: 10) // expected-error {{'let' property 'width' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
    self.height = height // expected-error {{cannot assign value of type 'Int' to type 'Measurement'}}
    self.init()
  }
}
