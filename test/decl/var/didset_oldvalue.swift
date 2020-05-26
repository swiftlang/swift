// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

import Swift

// SR-11297

// Do not call the getter to fetch the oldValue when oldValue is not
// referenced in the body
@propertyWrapper
struct Delayed<Value> {
  var wrappedValue: Value {
    get {
      guard let value = value else {
        preconditionFailure("Property \(String(describing: self)) has not been set yet")
      }
      return value
    }

    set {
      guard value == nil else {
        preconditionFailure("Property \(String(describing: self)) has already been set")
      }
      value = newValue
    }
  }
  
  var value: Value?
}

class Foo {
  @Delayed var bar: Int {
    // CHECK: Hello, world!
    didSet { print("Hello, world!") }
  }
}

let foo = Foo()
foo.bar = 1

// Call the getter to fetch the oldValue when oldValue is referenced in body
class AnotherFoo {
  var bar: Int = 0 {
    // CHECK: 0
    didSet { print(oldValue) }
  }
}

let anotherFoo = AnotherFoo()
anotherFoo.bar = 1

// Call the getter to fetch the oldValue when oldValue is explicitly specified
// as a parameter, but not used in the body.
class YetAnotherFoo {
  var bar: Int = 0 {
    // CHECK: World, hello!
    didSet(oldValue) { print("World, hello!") }
  }
}

let yetAnotherFoo = YetAnotherFoo()
yetAnotherFoo.bar = 1