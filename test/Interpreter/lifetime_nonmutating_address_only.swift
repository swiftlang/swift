// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// https://github.com/apple/swift/issues/51493

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

func testit() {
  let c = SomeClass()
  c.someProperty.x = 32
}

testit()

