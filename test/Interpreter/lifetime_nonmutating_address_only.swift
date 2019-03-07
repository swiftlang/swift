// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// SR-8990

// CHECK: A
// CHECK: B
// CHECK: C

protocol SomeProtocol { }
class SomeClass: SomeProtocol { deinit { print("C") } }
struct SomeStruct { var x, y: Int }

extension SomeProtocol {
    var someProperty: SomeStruct {
        nonmutating set {
          print("B")
        }
        get {
          print("A")
          return SomeStruct(x: 1, y: 2)
        }
    }
}

SomeClass().someProperty.x = 32
