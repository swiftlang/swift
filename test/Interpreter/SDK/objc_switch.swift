// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

func testAnyObjectIsa(obj: AnyObject) {
  switch obj {
  case is String:
    println("String")

  case is Int:
    println("Int")
    
  case is [NSString]:
    println("[NSString]")

  case is [Int]:
    println("[Int]")

  case is Dictionary<String, Int>:
    println("Dictionary<String, Int>")

  default:
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
  switch nsArr {
  case is [String]:
    println("[String]")

  case is [Int]:
    println("[Int]")

  default:
    println("Did not match");
  }
}

// CHECK: [String]
testNSArrayIsa(["a", "b", "c"])

// CHECK: [Int]
testNSArrayIsa([1, 2, 3])

// CHECK: Did not match
testNSArrayIsa([[1, 2], [3, 4], [5, 6]])

func testArrayIsa(arr: Array<AnyObject>) {
  switch arr {
  case is [NSString]:
    println("[NSString]")

  case is [NSNumber]:
    println("[NSNumber]")

  default:
    println("Did not match");
  }
}

// CHECK: [NSString]
testArrayIsa(["a", "b", "c"])

// CHECK: [NSNumber]
testArrayIsa([1, 2, 3])

// CHECK: Did not match
testArrayIsa([[1, 2], [3, 4], [5, 6]])

func testArrayIsaBridged(arr: Array<AnyObject>) {
  switch arr {
  case is [String]:
    println("[String]")

  case is [Int]:
    println("[Int]")

  default:
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
