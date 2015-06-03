// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

// Test dynamic casts that bridge value types through the runtime.

import Foundation

func genericForcedCast<T, U>(a: T) -> U {
  return a as! U
}

func genericConditionalCast<T, U>(a: T) -> U? {
  return a as? U
}

func testForcedValueToObjectBridging() {
  // CHECK: ---Forced value to object bridging---
  print("---Forced value to object bridging---")

  let array: [String] = ["Hello", "World"]

  // Forced bridging (exact)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  print(genericForcedCast(array) as NSArray)

  // Forced bridging (superclass)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  print(genericForcedCast(array) as NSObject)

  // Forced bridging (AnyObject)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  print(genericForcedCast(array) as NSObject)

  // Forced bridging (existential success)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  print(genericForcedCast(array) as NSCoding)

  print("Done")
}
// CHECK: Done
testForcedValueToObjectBridging()

func testConditionalValueToObjectBridging() {
  // CHECK: ---Conditional value to object bridging---
  print("---Conditional value to object bridging---")

  let array: [String] = ["Hello", "World"]

  // Conditional bridging (exact)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  if let nsArray = (genericConditionalCast(array) as NSArray?) {
    print("\(nsArray)")
  } else {
    print("Not an NSArray")
  }

  // Conditional bridging (superclass)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  if let nsObject = (genericConditionalCast(array) as NSObject?) {
    print("\(nsObject)")
  } else {
    print("Not an NSObject")
  }

  // Conditional bridging (AnyObject)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  if let anyObject = (genericConditionalCast(array) as AnyObject?) {
    print("\(anyObject)")
  } else {
    print("Not an AnyObject")
  }

  // Conditional bridging (existential success)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  if let coding = (genericConditionalCast(array) as NSCoding?) {
    print("\(coding)")
  } else {
    print("Not an NSCoding")
  }

  // CHECK-NEXT: NSXMLParserDelegate
  if let delegate = (genericConditionalCast(array) as NSXMLParserDelegate?) {
    print("\(delegate)")
  } else {
    print("Not an NSXMLParserDelegate")
  }

  // Conditional bridging (unrelated class)
  // CHECK-NEXT: Not an NSString
  if let nsString = (genericConditionalCast(array) as NSString?) {
    print("\(nsString)")
  } else {
    print("Not an NSString")
  }

  print("Done")
}
// CHECK: Done
testConditionalValueToObjectBridging()

func testForcedObjectToValueBridging() {
  // CHECK: ---Forced object to value bridging---
  print("---Forced object to value bridging---")

  let nsArray: NSArray = ["Hello", "World"]

  // Forced bridging (exact)
  // CHECK: [Hello, World]
  print(genericForcedCast(nsArray) as [String])

  // Forced bridging (superclass)
  // CHECK: [Hello, World]
  let nsObject: NSObject = nsArray
  print(genericForcedCast(nsObject) as [String])

  // Forced bridging (AnyObject)
  // CHECK: [Hello, World]
  let anyObject: AnyObject = nsArray
  print(genericForcedCast(anyObject) as [String])

  // Forced bridging (existential success)
  let nsCoding: NSCoding = nsArray
  print(genericForcedCast(nsCoding) as [String])

  print("Done")
}
// CHECK: Done
testForcedObjectToValueBridging()

func testConditionalObjectToValueBridging() {
  // CHECK: ---Conditional object to value bridging---
  print("---Conditional object to value bridging---")

  let nsArray: NSArray = ["Hello", "World"]
  let nsObject: NSObject = nsArray
  let anyObject: AnyObject = nsArray
  let nsCoding: NSCoding = nsArray
  let nsString: NSString = "Hello"

  // Conditional bridging (exact)
  // CHECK: [Hello, World]
  if let arr = (genericConditionalCast(nsArray) as [String]?) {
    print(arr)
  } else {
    print("Not a [String]")
  }

  // Conditional bridging (superclass)
  // CHECK: [Hello, World]
  if let arr = (genericConditionalCast(nsObject) as [String]?) {
    print(arr)
  } else {
    print("Not a [String]")
  }

  // Conditional bridging (AnyObject)
  // CHECK: [Hello, World]
  if let arr = (genericConditionalCast(anyObject) as [String]?) {
    print(arr)
  } else {
    print("Not a [String]")
  }

  // Conditional bridging (existential success)
  // CHECK: [Hello, World]
  if let arr = (genericConditionalCast(nsCoding) as [String]?) {
    print(arr)
  } else {
    print("Not a [String]")
  }

  // Conditional bridging (existential failure)
  // Not a [Int]
  if let arr = (genericConditionalCast(nsCoding) as [Int]?) {
    print(arr)
  } else {
    print("Not a [String]")
  }

  // Conditional bridging (unrelated class type)
  // CHECK: Not a [String]
  if let arr = (genericConditionalCast(nsString) as [String]?) {
    print(arr)
  } else {
    print("Not a [String]")
  }

  // Conditional bridging (unrelated element type)
  // CHECK: Not a [Int]
  if let arr = (genericConditionalCast(nsArray) as [Int]?) {
    print(arr)
  } else {
    print("Not a [Int]")
  }

  print("Done")
}
// CHECK: Done
testConditionalObjectToValueBridging()
