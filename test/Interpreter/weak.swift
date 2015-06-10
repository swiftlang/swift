// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// XFAIL: linux

protocol Protocol : class {
  func noop()
}

//========================== Test pure Swift classes ==========================

class SwiftClassBase : Protocol {
  func noop() { print("noop") }
}

class SwiftClass : SwiftClassBase {
  override init() {
    print("SwiftClass Created")
  }

  deinit {
    print("SwiftClass Destroyed")
  }
}

func printState(x : SwiftClassBase?) {
  print((x != nil) ? "is present" : "is nil")
}

func testSwiftClass() {
  print("testSwiftClass")               // CHECK: testSwiftClass
  
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
  print("testSwiftImplicitOptionalClass") // CHECK: testSwiftImplicitOptionalClass
  
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


//======================== Test Classbound Protocols ========================


  
func printState(x : Protocol?) {
  print((x != nil) ? "is present" : "is nil")
}

func testProtocol() {
  print("testProtocol")                 // CHECK: testProtocol
  
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




protocol P : class { }

func test_rdar15293354() {
  weak var x : P? = .None
  if x != nil {
    _ = x!
  }
}

test_rdar15293354()

