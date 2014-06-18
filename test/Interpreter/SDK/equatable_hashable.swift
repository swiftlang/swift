// RUN: %target-run-simple-swift | FileCheck %s

import ObjectiveC
import Foundation

func testHash<H: Hashable>(x: H) -> Int { return x.hashValue }

func test_CBool() {
  let x: CBool = true
  let hash = testHash(x)
  println("C_Bool: hash = \(hash)")
}
// CHECK: C_Bool: hash = 1
test_CBool()

func test_CString() {
  let x: CString = "hello"
  let y: CString = "hello"
  let z: CString = "world"  
  let x_s: String = "hello"
  let hash = testHash(x)
  println("CString hash equals String hash? \(hash == x_s.hashValue)")
  println("CString equality test: \(x == y)")
  println("CString comparison test: \(x < z)")
  println("CString comparison test 2: \(z < x)")
}
// CHECK-NEXT: CString hash equals String hash? true
// CHECK-NEXT: CString equality test: true
// CHECK-NEXT: CString comparison test: true
// CHECK-NEXT: CString comparison test 2: false
test_CString()

func test_ObjCBool() {
  let x: ObjCBool = true
  let hash = testHash(x as Bool)
  println("ObjCBool: hash = \(hash)")
}
// CHECK-NEXT: ObjCBool: hash = 1
test_ObjCBool()

func test_Word() {
  let x: Word = 42
  let hash = testHash(x)
  println("Word: hash = \(hash)")
}
// CHECK-NEXT: Word: hash = 42
test_Word()

func test_UWord() {
  let x: UWord = 42
  let hash = testHash(x)
  println("UWord: hash = \(hash)")
}
// CHECK-NEXT: UWord: hash = 42
test_UWord()

func testEquatable<E: Equatable>(x: E) {}

func test_Equatable() {
  // CHECK-NEXT: Found 2.5 at index 1
  let array: NSNumber[] = [1, 2.5, 3.14159]
  if let index = find(array, 2.5) {
    println("Found \(array[index]) at index \(index)")
  } else {
    println("Did not find 2.5?")
  }

  testEquatable(array[1])
}
test_Equatable()
