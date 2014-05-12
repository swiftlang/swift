// RUN: %target-run-simple-swift | FileCheck %s

import ObjectiveC

func test_CBool() {
  let x: CBool = true
  let p: protocol<Equatable, Hashable> = x
  let hash = p.hashValue
  println("C_Bool: hash = \(hash)")
}
// CHECK: C_Bool: hash = 1
test_CBool()

func test_CString() {
  let x: CString = "hello"
  let y: CString = "hello"
  let z: CString = "world"  
  let x_s: String = "hello"
  let p: protocol<Equatable, Hashable, Comparable> = x
  let hash = p.hashValue
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
  let p: protocol<Equatable, Hashable> = x
  let hash = p.hashValue
  println("ObjCBool: hash = \(hash)")
}
// CHECK-NEXT: ObjCBool: hash = 1
test_ObjCBool()

func test_Word() {
  let x: Word = 42
  let p: protocol<Equatable, Comparable, Hashable> = x
  let hash = p.hashValue
  println("Word: hash = \(hash)")
}
// CHECK-NEXT: Word: hash = 42
test_Word()

func test_UWord() {
  let x: UWord = 42
  let p: protocol<Equatable, Comparable, Hashable> = x
  let hash = p.hashValue
  println("UWord: hash = \(hash)")
}
// CHECK-NEXT: UWord: hash = 42
test_UWord()

