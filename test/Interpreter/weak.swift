// RUN: %target-run-simple-swift | FileCheck %s

import ObjectiveC

@class_protocol protocol Protocol {
  func noop()
}

//========================== Test pure Swift classes ==========================

class SwiftClassBase : Protocol {
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
  printState(w)                           // CHECK-NEXT: is nil
}

testSwiftClass()



func testSwiftImplicitOptionalClass() {
  println("testSwiftImplicitOptionalClass") // CHECK: testSwiftImplicitOptionalClass
  
  weak var w : SwiftClassBase!
  printState(w)                           // CHECK-NEXT: is nil
  var c : SwiftClassBase = SwiftClass()   // CHECK: SwiftClass Created
  printState(w)                           // CHECK-NEXT: is nil
  w = c
  printState(w)                           // CHECK-NEXT: is present
  c.noop()                                // CHECK-NEXT: noop
  c = SwiftClassBase()                    // CHECK-NEXT: SwiftClass Destroyed
  printState(w)                           // CHECK-NEXT: is nil
}

testSwiftImplicitOptionalClass()

//========================== Test ObjC classes ==========================

@objc
class ObjCClassBase : Protocol {
  func noop() { println("noop") }
}

@objc
class ObjCClass : ObjCClassBase {
  init() {
    println("ObjCClass Created")
  }

  deinit {
    println("ObjCClass Destroyed")
  }
}

func printState(x : ObjCClassBase?) {
  println(x ? "is present" : "is nil")
}

func testObjCClass() {
  println("testObjCClass")                // CHECK: testObjCClass
  
  weak var w : ObjCClassBase?
  printState(w)                           // CHECK-NEXT: is nil
  var c : ObjCClassBase = ObjCClass()     // CHECK: ObjCClass Created
  printState(w)                           // CHECK-NEXT: is nil
  w = c
  printState(w)                           // CHECK-NEXT: is present
  c.noop()                                // CHECK-NEXT: noop
  c = ObjCClassBase()                     // CHECK-NEXT: ObjCClass Destroyed
  printState(w)                           // CHECK-NEXT: is nil
}

testObjCClass()


//======================== Test Classbound Protocols ========================


  
func printState(x : Protocol?) {
  println(x ? "is present" : "is nil")
}

func testProtocol() {
  println("testProtocol")                 // CHECK: testProtocol
  
  weak var w : Protocol?
  printState(w)                           // CHECK-NEXT: is nil
  var c : SwiftClassBase = SwiftClass()   // CHECK: SwiftClass Created
  printState(w)                           // CHECK-NEXT: is nil
  w = c
  printState(w)                           // CHECK-NEXT: is present
  c.noop()                                // CHECK-NEXT: noop
  c = SwiftClassBase()                    // CHECK-NEXT: SwiftClass Destroyed
  printState(w)                           // CHECK-NEXT: is nil
}

testProtocol()




@class_protocol protocol P { }

func test_rdar15293354() {
  weak var x : P? = .None
  if x {
    x!
  }
}

test_rdar15293354()

