// RUN: %target-run-simple-swift | FileCheck %s

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
  println("---Forced value to object bridging---")

  let array: [String] = ["Hello", "World"]

  // Forced bridging (exact)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  println(genericForcedCast(array) as NSArray)

  // Forced bridging (superclass)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  println(genericForcedCast(array) as NSObject)

  // Forced bridging (AnyObject)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  println(genericForcedCast(array) as NSObject)

  // Forced bridging (existential success)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  println(genericForcedCast(array) as NSCoding)

  println("Done")
}
// CHECK: Done
testForcedValueToObjectBridging()

func testConditionalValueToObjectBridging() {
  // CHECK: ---Conditional value to object bridging---
  println("---Conditional value to object bridging---")

  let array: [String] = ["Hello", "World"]

  // Conditional bridging (exact)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  if let nsArray = (genericConditionalCast(array) as NSArray?) {
    println("\(nsArray)")
  } else {
    println("Not an NSArray")
  }

  // Conditional bridging (superclass)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  if let nsObject = (genericConditionalCast(array) as NSObject?) {
    println("\(nsObject)")
  } else {
    println("Not an NSObject")
  }

  // Conditional bridging (AnyObject)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  if let anyObject = (genericConditionalCast(array) as AnyObject?) {
    println("\(anyObject)")
  } else {
    println("Not an AnyObject")
  }

  // Conditional bridging (existential success)
  // CHECK-NEXT: (
  // CHECK-NEXT:   Hello,
  // CHECK-NEXT:   World
  // CHECK-NEXT: )
  if let coding = (genericConditionalCast(array) as NSCoding?) {
    println("\(coding)")
  } else {
    println("Not an NSCoding")
  }

  // CHECK-NEXT: NSXMLParserDelegate
  if let delegate = (genericConditionalCast(array) as NSXMLParserDelegate?) {
    println("\(delegate)")
  } else {
    println("Not an NSXMLParserDelegate")
  }

  // Conditional bridging (unrelated class)
  // CHECK-NEXT: Not an NSString
  if let nsString = (genericConditionalCast(array) as NSString?) {
    println("\(nsString)")
  } else {
    println("Not an NSString")
  }

  println("Done")
}
// CHECK: Done
testConditionalValueToObjectBridging()

func testForcedObjectToValueBridging() {
  // CHECK: ---Forced object to value bridging---
  println("---Forced object to value bridging---")

  let nsArray: NSArray = ["Hello", "World"]

  // Forced bridging (exact)
  // CHECK: [Hello, World]
  println(genericForcedCast(nsArray) as [String])

  // Forced bridging (superclass)
  // CHECK: [Hello, World]
  let nsObject: NSObject = nsArray
  println(genericForcedCast(nsObject) as [String])

  // Forced bridging (AnyObject)
  // CHECK: [Hello, World]
  let anyObject: AnyObject = nsArray
  println(genericForcedCast(anyObject) as [String])

  // Forced bridging (existential success)
  let nsCoding: NSCoding = nsArray
  println(genericForcedCast(nsCoding) as [String])

  println("Done")
}
// CHECK: Done
testForcedObjectToValueBridging()

func testConditionalObjectToValueBridging() {
  // CHECK: ---Conditional object to value bridging---
  println("---Conditional object to value bridging---")

  let nsArray: NSArray = ["Hello", "World"]
  let nsObject: NSObject = nsArray
  let anyObject: AnyObject = nsArray
  let nsCoding: NSCoding = nsArray
  let nsString: NSString = "Hello"

  // Conditional bridging (exact)
  // CHECK: [Hello, World]
  if let arr = (genericConditionalCast(nsArray) as [String]?) {
    println(arr)
  } else {
    println("Not a [String]")
  }

  // Conditional bridging (superclass)
  // CHECK: [Hello, World]
  if let arr = (genericConditionalCast(nsObject) as [String]?) {
    println(arr)
  } else {
    println("Not a [String]")
  }

  // Conditional bridging (AnyObject)
  // CHECK: [Hello, World]
  if let arr = (genericConditionalCast(anyObject) as [String]?) {
    println(arr)
  } else {
    println("Not a [String]")
  }

  // Conditional bridging (existential success)
  // CHECK: [Hello, World]
  if let arr = (genericConditionalCast(nsCoding) as [String]?) {
    println(arr)
  } else {
    println("Not a [String]")
  }

  // Conditional bridging (existential failure)
  // Not a [Int]
  if let arr = (genericConditionalCast(nsCoding) as [Int]?) {
    println(arr)
  } else {
    println("Not a [String]")
  }

  // Conditional bridging (unrelated class type)
  // CHECK: Not a [String]
  if let arr = (genericConditionalCast(nsString) as [String]?) {
    println(arr)
  } else {
    println("Not a [String]")
  }

  // Conditional bridging (unrelated element type)
  // CHECK: Not a [Int]
  if let arr = (genericConditionalCast(nsArray) as [Int]?) {
    println(arr)
  } else {
    println("Not a [Int]")
  }

  println("Done")
}
// CHECK: Done
testConditionalObjectToValueBridging()
