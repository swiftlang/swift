// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

func testAnyObjectIsa(_ obj: AnyObject) {
  print("(", terminator: "")
  if obj is String {
    print("String", terminator: "")
  }
  if obj is Int {
    print("Int", terminator: "")
  }
  if obj is [NSString] {
    print("[NSString]", terminator: "")
  }
  if obj is [Int] {
    print("[Int]", terminator: "")
  }
  if obj is Dictionary<String, Int> {
    print("Dictionary<String, Int>", terminator: "")
  }
  print(")")
}

// CHECK: testing...
print("testing...")


// CHECK-NEXT: (String)
testAnyObjectIsa("hello" as NSString)

// CHECK-NEXT: (Int)
testAnyObjectIsa(5 as NSNumber)

// CHECK-NEXT: ([NSString])
testAnyObjectIsa(["hello", "swift", "world"] as NSArray)

// CHECK-NEXT: ([Int])
testAnyObjectIsa([1, 2, 3, 4, 5] as NSArray)

// CHECK-NEXT: (Dictionary<String, Int>)
testAnyObjectIsa(["hello" : 1, "world" : 2] as NSDictionary)

func testNSArrayIsa(_ nsArr: NSArray) {
  print("(", terminator: "")
  if nsArr is [String] {
    print("[String]", terminator: "")
  }
  if nsArr is [Int] {
    print("[Int]", terminator: "")
  }
  print(")")
}

// CHECK-NEXT: ([String])
testNSArrayIsa(["a", "b", "c"])

// CHECK-NEXT: ([Int])
testNSArrayIsa([1, 2, 3])

// CHECK-NEXT: ()
testNSArrayIsa([[1, 2], [3, 4], [5, 6]])

func testArrayIsa(_ arr: Array<Any>) {
  print("(", terminator: "")
  if arr is [NSString] {
    print("[NSString]", terminator: "")
  }
  if arr is [NSNumber] {
    print("[NSNumber]", terminator: "")
  }
  print(")")
}

// CHECK-NEXT: ([NSString])
testArrayIsa(["a", "b", "c"])

// CHECK-NEXT: ([NSNumber])
testArrayIsa([1, 2, 3])

// CHECK-NEXT: ()
testArrayIsa([[1, 2], [3, 4], [5, 6]])

func testArrayIsaBridged(_ arr: Array<Any>) {
  print("(", terminator: "")
  if arr is [String] {
    print("[String]", terminator: "")
  }
  if arr is [Int] {
    print("[Int]", terminator: "")
  }
  print(")")
}

// CHECK-NEXT: ([String])
testArrayIsaBridged(["a", "b", "c"])

// CHECK-NEXT: ([Int])
testArrayIsaBridged([1, 2, 3])

// CHECK-NEXT: ()
testArrayIsaBridged([[1, 2], [3, 4], [5, 6]])

func testNSMutableStringMatch(_ sa: NSMutableString) {
  switch(sa) {
  case "foobar":
    print("MATCH")
  default:
    print("nomatch")
  }
}

// CHECK-NEXT: MATCH
testNSMutableStringMatch("foobar")

// CHECK-NEXT: nomatch
testNSMutableStringMatch("nope")

func testAnyObjectDowncast(_ obj: AnyObject!) {
  switch obj {
  case let str as String:
    print("String: \(str)")

  case let int as Int:
    print("Int: \(int)")
    
  case let nsStrArr as [NSString]:
    print("NSString array: \(nsStrArr)")

  case let intArr as [Int]:
    print("Int array: \(intArr)")

  case let dict as Dictionary<String, Int>:
    print("Dictionary<String, Int>: \(dict)")

  default:
    print("Did not match")
  }
}

// CHECK-NEXT: String: hello
testAnyObjectDowncast("hello" as NSString)

// CHECK-NEXT: Int: 5
testAnyObjectDowncast(5 as NSNumber)

// CHECK-NEXT: NSString array: [hello, swift, world]
testAnyObjectDowncast(["hello", "swift", "world"] as NSArray)

// CHECK-NEXT: Int array: [1, 2, 3, 4, 5]
testAnyObjectDowncast([1, 2, 3, 4, 5] as NSArray)

// CHECK: Dictionary<String, Int>: [
// CHECK-DAG: "hello": 1
// CHECK-DAG: "world": 2
// CHECK: ]
testAnyObjectDowncast(["hello" : 1, "world" : 2] as NSDictionary)

// CHECK-NEXT: Did not match
testAnyObjectDowncast(nil)

func testNSArrayDowncast(_ nsArr: NSArray?) {
  switch nsArr {
  case let strArr as [String]:
    print("[String]: \(strArr)")

  case let intArr as [Int]:
    print("[Int]: \(intArr)")

  default:
    print("Did not match");
  }
}

// CHECK-NEXT: [String]: ["a", "b", "c"]
testNSArrayDowncast(["a", "b", "c"])

// CHECK-NEXT: [Int]: [1, 2, 3]
testNSArrayDowncast([1, 2, 3])

// CHECK-NEXT: Did not match
testNSArrayDowncast([[1, 2], [3, 4], [5, 6]])

// CHECK-NEXT: Did not match
testNSArrayDowncast(nil)
