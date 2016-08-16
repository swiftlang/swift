// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import ObjectiveC
import Foundation

func testHash<H: Hashable>(_ x: H) -> Int { return x.hashValue }

func test_CBool() {
  let x: CBool = true
  let hash = testHash(x)
  print("C_Bool: hash = \(hash)")
}
// CHECK: C_Bool: hash = 1
test_CBool()

func test_ObjCBool() {
  let x = ObjCBool(true)
  let hash = testHash(x.boolValue)
  print("ObjCBool: hash = \(hash)")
}
// CHECK-NEXT: ObjCBool: hash = 1
test_ObjCBool()

func testEquatable<E: Equatable>(_ x: E) {}

func test_Equatable() {
  // CHECK-NEXT: Found 2.5 at index 1
  let array: [NSNumber] = [1, 2.5, 3.14159]
  if let index = array.index(of: 2.5) {
    print("Found \(array[index]) at index \(index)")
  } else {
    print("Did not find 2.5?")
  }

  testEquatable(array[1])
}
test_Equatable()
