// RUN: %target-parse-verify-swift

// Simple struct types
struct X1 {
  static func create(i: Int) -> X1 {  }
  static func createGeneric<T>(d: T) -> X1 {  }
  static func createMaybe(i : Int) -> X1! { }

  static func notAFactory(i: Int) -> Int { }
}

// Static methods
var x1: X1 = .create(5)
x1 = .createGeneric(3.14159)

// Non-matching data members
x1 = .notAFactory(5) // expected-error{{could not find member 'notAFactory'}}

// Static methods returning unchecked-optional types
x1 = .createMaybe(5)

// Generic struct types
struct X2<T> {
  static func create(t: T) -> X2 {  }
  static func createGeneric<U>(t: T, u:U) -> X2 {  }
  static func createMaybe(i : T) -> X2! { }
}

// Static methods
var x2a: X2 = .create(5)
x2a = .createGeneric(5, u: 3.14159)
var x2b: X2 = .createGeneric(5, u: 3.14159)
var x2c: X2 = .createMaybe(5)
