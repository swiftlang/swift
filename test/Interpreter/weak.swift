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


func testWeakInLet() {
  print("testWeakInLet") // CHECK-LABEL: testWeakInLet

  struct WeakBox {
    weak var value: SwiftClassBase?
  }

  var obj: SwiftClassBase? = SwiftClass() // CHECK: SwiftClass Created
  let box = WeakBox(value: obj)
  printState(box.value) // CHECK-NEXT: is present
  obj = nil // CHECK-NEXT: SwiftClass Destroyed
  printState(box.value) // CHECK-NEXT: is nil
}

testWeakInLet()

func testWeakLet() {
  print("testWeakLet") // CHECK-LABEL: testWeakLet

  var obj: SwiftClassBase? = SwiftClass() // CHECK: SwiftClass Created
  weak let weakRef = obj
  printState(weakRef) // CHECK-NEXT: is present
  obj = nil // CHECK-NEXT: SwiftClass Destroyed
  printState(weakRef) // CHECK-NEXT: is nil
}

testWeakLet()


//======================== Test Classbound Protocols ========================


  
func printState(_ x : Protocol?) {
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
  weak var x : P? = .none
  if x != nil {
    _ = x!
  }
}

test_rdar15293354()




func testStaticObject() {
  print("testStaticObject")                     // CHECK: testStaticObject

  enum Static {
    static let staticObject = SwiftClassBase()
  }
  weak var w: SwiftClassBase?
  printState(w)                                 // CHECK-NEXT: is nil
  w = Static.staticObject
  printState(w)                                 // CHECK-NEXT: is present
}

testStaticObject()
