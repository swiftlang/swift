// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

func testAnyObjectIsa(obj: AnyObject) {

  if obj is String {
    println("String")
  }
  else if obj is Int {
    println("Int")
  }
  else if obj is [NSString] {
    println("[NSString]")
  }
  else if obj is [Int] {
    println("[Int]")
  }
  else if obj is Dictionary<String, Int> {
    println("Dictionary<String, Int>")
  }
  else {
    println("Did not match")
  }
}

// CHECK: String
testAnyObjectIsa("hello")

// CHECK: Int
testAnyObjectIsa(5)

// CHECK: [NSString]
testAnyObjectIsa(["hello", "swift", "world"])

// CHECK: [Int]
testAnyObjectIsa([1, 2, 3, 4, 5])

// CHECK: Dictionary<String, Int>
testAnyObjectIsa(["hello" : 1, "world" : 2])

func testNSArrayIsa(nsArr: NSArray) {
  if nsArr is [String] {
    println("[String]")
  }
  else if nsArr is [Int] {
    println("[Int]")
  }
  else {
    println("Did not match")
  }
}

// CHECK: [String]
testNSArrayIsa(["a", "b", "c"])

// CHECK: [Int]
testNSArrayIsa([1, 2, 3])

// CHECK: Did not match
testNSArrayIsa([[1, 2], [3, 4], [5, 6]])

func testArrayIsa(arr: Array<AnyObject>) {
  if arr is [NSString] {
    println("[NSString]")
  }
  else if arr is [NSNumber] {
    println("[NSNumber]")
  }
  else {
    println("Did not match")
  }
}

// CHECK: [NSString]
testArrayIsa(["a", "b", "c"])

// CHECK: [NSNumber]
testArrayIsa([1, 2, 3])

// CHECK: Did not match
testArrayIsa([[1, 2], [3, 4], [5, 6]])

func testArrayIsaBridged(arr: Array<AnyObject>) {
  if arr is [String] {
    println("[String]")
  }
  else if arr is [Int] {
    println("[Int]")
  }
  else {
    println("Did not match");
  }
}

// CHECK: [String]
testArrayIsaBridged(["a", "b", "c"])

// CHECK: [Int]
testArrayIsaBridged([1, 2, 3])

// CHECK: Did not match
testArrayIsaBridged([[1, 2], [3, 4], [5, 6]])

func testNSMutableStringMatch(sa: NSMutableString) {
  switch(sa) {
  case "foobar":
    println("MATCH")
  default:
    println("nomatch")
  }
}

// CHECK: MATCH
testNSMutableStringMatch("foobar")

// CHECK: nomatch
testNSMutableStringMatch("nope")
