// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

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

func printState(_ x : SwiftClassBase?) {
  print((x != nil) ? "is present" : "is nil")
}

func testSwiftClass() {
  print("testSwiftClass")               // CHECK: testSwiftClass
  
  unowned var w : SwiftClassBase?
  printState(w)                           // CHECK-NEXT: is nil
  var c : SwiftClassBase = SwiftClass()   // CHECK: SwiftClass Created
  printState(w)                           // CHECK-NEXT: is nil
  w = c
  printState(w)                           // CHECK-NEXT: is present
  c.noop()                                // CHECK-NEXT: noop
  c = SwiftClassBase()                    // CHECK-NEXT: SwiftClass Destroyed
}

testSwiftClass()



func testSwiftImplicitOptionalClass() {
  print("testSwiftImplicitOptionalClass") // CHECK: testSwiftImplicitOptionalClass
  
  unowned var w : SwiftClassBase!
  printState(w)                           // CHECK-NEXT: is nil
  var c : SwiftClassBase = SwiftClass()   // CHECK: SwiftClass Created
  printState(w)                           // CHECK-NEXT: is nil
  w = c
  printState(w)                           // CHECK-NEXT: is present
  c.noop()                                // CHECK-NEXT: noop
  c = SwiftClassBase()                    // CHECK-NEXT: SwiftClass Destroyed
}

testSwiftImplicitOptionalClass()


func testWeakInLet() {
  print("testWeakInLet") // CHECK-LABEL: testWeakInLet

  struct WeakBox {
    unowned var value: SwiftClassBase?
  }

  var obj: SwiftClassBase? = SwiftClass() // CHECK: SwiftClass Created
  let box = WeakBox(value: obj)
  printState(box.value) // CHECK-NEXT: is present
  obj = nil // CHECK-NEXT: SwiftClass Destroyed
}

testWeakInLet()


//======================== Test Classbound Protocols ========================


  
func printState(_ x : Protocol?) {
  print((x != nil) ? "is present" : "is nil")
}

func testProtocol() {
  print("testProtocol")                 // CHECK: testProtocol
  
  unowned var w : Protocol?
  printState(w)                           // CHECK-NEXT: is nil
  var c : SwiftClassBase = SwiftClass()   // CHECK: SwiftClass Created
  printState(w)                           // CHECK-NEXT: is nil
  w = c
  printState(w)                           // CHECK-NEXT: is present
  c.noop()                                // CHECK-NEXT: noop
  c = SwiftClassBase()                    // CHECK-NEXT: SwiftClass Destroyed
}

testProtocol()
