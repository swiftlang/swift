// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import ObjectiveC
import Foundation

func testHash<H: Hashable>(x: H) -> Int { return x.hashValue }

func test_CBool() {
  let x: CBool = true
  let hash = testHash(x)
  print("C_Bool: hash = \(hash)")
}
// CHECK: C_Bool: hash = 1
test_CBool()

func test_ObjCBool() {
  let x = ObjCBool(true)
  let hash = testHash(Bool(x))
  print("ObjCBool: hash = \(hash)")
}
// CHECK-NEXT: ObjCBool: hash = 1
test_ObjCBool()

func test_Word() {
  let x: Word = 42
  let hash = testHash(x)
  print("Word: hash = \(hash)")
}
// CHECK-NEXT: Word: hash = 42
test_Word()

func test_UWord() {
  let x: UWord = 42
  let hash = testHash(x)
  print("UWord: hash = \(hash)")
}
// CHECK-NEXT: UWord: hash = 42
test_UWord()

func testEquatable<E: Equatable>(x: E) {}

func test_Equatable() {
  // CHECK-NEXT: Found 2.5 at index 1
  let array: [NSNumber] = [1, 2.5, 3.14159]
  if let index = array.indexOf(2.5) {
    print("Found \(array[index]) at index \(index)")
  } else {
    print("Did not find 2.5?")
  }

  testEquatable(array[1])
}
test_Equatable()
