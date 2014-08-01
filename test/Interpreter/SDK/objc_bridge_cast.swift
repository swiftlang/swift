// RUN: %target-run-simple-swift | FileCheck %s

// Test dynamic casts that bridge value types through the runtime.

import Foundation

func genericForcedCast<T, U>(a: T) -> U {
  return a as U
}

func genericConditionalCast<T, U>(a: T) -> U? {
  return a as? U
}

func testForcedValueToObjectBridging() {
  // CHECK: ---Forced value to object bridging---
  println("---Forced value to object bridging---")
  
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

  // FIXME: Conditional bridging (AnyObject)
  // FIXME: Conditional bridging (existential success)
  // FIXME: Conditional bridging (existential failure)

  // Conditional bridging (unrelated class)
  // CHECK-NEXT: Not an NSString
  if let nsString = (genericConditionalCast(array) as NSString?) {
    println("\(nsString)")
  } else {
    println("Not an NSString")
  }

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

  // FIXME: Forced bridging (AnyObject)
  // FIXME: Forced bridging (existential success)

  println("Done")
}
// CHECK: Done
testForcedValueToObjectBridging()
