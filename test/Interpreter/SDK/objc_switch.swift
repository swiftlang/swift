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

func testAnyObjectDowncast(obj: AnyObject!) {
  switch obj {
  case let str as String:
    println("String: \(str)")

  case let int as Int:
    println("Int: \(int)")
    
  case let nsStrArr as [NSString]:
    println("NSString array: \(nsStrArr)")

  case let intArr as [Int]:
    println("Int array: \(intArr)")

  case let dict as Dictionary<String, Int>:
    println("Dictionary<String, Int>: \(dict)")

  default:
    println("Did not match")
  }
}

// CHECK: String: hello
testAnyObjectDowncast("hello")

// CHECK: Int: 5
testAnyObjectDowncast(5)

// CHECK: NSString array: [hello, swift, world]
testAnyObjectDowncast(["hello", "swift", "world"] as NSArray)

// CHECK: Int array: [1, 2, 3, 4, 5]
testAnyObjectDowncast([1, 2, 3, 4, 5])

// CHECK: Dictionary<String, Int>: [
// CHECK-DAG: hello: 1
// CHECK-DAG: world: 2
// CHECK: ]
testAnyObjectDowncast(["hello" : 1, "world" : 2])

// CHECK: Did not match
testAnyObjectDowncast(nil)

func testNSArrayDowncast(nsArr: NSArray?) {
  switch nsArr {
  case let strArr as [String]:
    println("[String]: \(strArr)")

  case let intArr as [Int]:
    println("[Int]: \(intArr)")

  default:
    println("Did not match");
  }
}

// CHECK: [String]: [a, b, c]
testNSArrayDowncast(["a", "b", "c"])

// CHECK: [Int]: [1, 2, 3]
testNSArrayDowncast([1, 2, 3])

// CHECK: Did not match
testNSArrayDowncast([[1, 2], [3, 4], [5, 6]])

// CHECK: Did not match
testNSArrayDowncast(nil)
