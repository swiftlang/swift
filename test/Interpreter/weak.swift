// RUN: %target-run-simple-swift | FileCheck %s

import ObjectiveC

class SwiftClassBase {
  func noop() { println("noop") }
}

class SwiftClass : SwiftClassBase {
  init() {
    println("SwiftClass Created")
  }

  deinit {
    println("SwiftClass Destroyed")
  }
}

func printState(x : SwiftClassBase?) {
  println(x ? "is present" : "is nil")
}

func testSwiftClass() {
  println("testSwiftClass")               // CHECK: testSwiftClass
  
  weak var w : SwiftClassBase?
  printState(w)                           // CHECK-NEXT: is nil
  var c : SwiftClassBase = SwiftClass()   // CHECK: SwiftClass Created
  printState(w)                           // CHECK-NEXT: is nil
  w = c
  printState(w)                           // CHECK-NEXT: is present
  c.noop()                                // CHECK-NEXT: noop
  c = SwiftClassBase()                    // CHECK-NEXT: SwiftClass Destroyed
  w = nil
  printState(w)                           // CHECK-NEXT: is nil
}

testSwiftClass()



