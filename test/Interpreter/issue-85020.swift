// RUN: %target-run-simple-swift | %FileCheck %s

// https://github.com/swiftlang/swift/issues/85020

// REQUIRES: executable_test

struct Store {
  let theType: Any.Type

  init(of theType: Any.Type) {
    print("init from TYPE: \(theType)")
    self.theType = theType
  }

  init(of instance: Any) {
    print("init from VALUE: \(instance)")
    self.init(of: type(of: instance))
  }
}

let a: (any Numeric)? = 42
print("a: \(type(of: a))")
// CHECK: a: Optional<Numeric>

let storeA = Store(of: a!)
// CHECK-NEXT: init from VALUE: 42
// CHECK-NEXT: init from TYPE: Int

let b: (any Numeric.Type)? = type(of: 42)
print("b: \(type(of: b))")
// CHECK-NEXT: b: Optional<Numeric.Type>

let storeB = Store(of: b!)
// CHECK-NEXT: init from TYPE: Int

