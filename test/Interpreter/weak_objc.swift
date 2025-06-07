// RUN: %target-build-swift %s -Xfrontend -disable-objc-attr-requires-foundation-module -enable-experimental-feature WeakLet -o %t-main
// RUN: %target-codesign %t-main
// RUN: %target-run %t-main | %FileCheck %s

// RUN: %target-build-swift %s -Xfrontend -disable-objc-attr-requires-foundation-module -enable-experimental-feature WeakLet -o %t-main-weak-let
// RUN: %target-codesign %t-main-weak-let
// RUN: %target-run %t-main-weak-let | %FileCheck %s --check-prefixes=CHECK,CHECK-WEAK-LET

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

protocol Protocol : class {
  func noop()
}

//========================== Test ObjC classes ==========================

@objc
class ObjCClassBase : Protocol {
  func noop() { print("noop") }
}

@objc
class ObjCClass : ObjCClassBase {
  override init() {
    print("ObjCClass Created")
  }

  deinit {
    print("ObjCClass Destroyed")
  }
}

func printState(_ x : ObjCClassBase?) {
  print((x != nil) ? "is present" : "is nil")
}

func testObjCClass() {
  print("testObjCClass")                // CHECK: testObjCClass
  
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

#if hasFeature(WeakLet)
func testObjCWeakLet() {
  print("testObjCWeakLet")                // CHECK-WEAK-LET: testObjCWeakLet

  var c : ObjCClassBase = ObjCClass()     // CHECK-WEAK-LET: ObjCClass Created
  weak let w : ObjCClassBase? = c
  printState(w)                           // CHECK-WEAK-LET-NEXT: is present
  c = ObjCClassBase()                     // CHECK-WEAK-LET-NEXT: ObjCClass Destroyed
  printState(w)                           // CHECK-WEAK-LET-NEXT: is nil
}

testObjCWeakLet()

func testObjCWeakLetCapture() {
  print("testObjCWeakLetCapture")         // CHECK-WEAK-LET: testObjCWeakLetCapture

  var c : ObjCClassBase = ObjCClass()     // CHECK-WEAK-LET: ObjCClass Created
  let closure: () -> ObjCClassBase? = { [weak c] in c }
  printState(closure())                   // CHECK-WEAK-LET-NEXT: is present
  printState(closure())                   // CHECK-WEAK-LET-NEXT: is present
  c = ObjCClassBase()                     // CHECK-WEAK-LET-NEXT: ObjCClass Destroyed
  printState(closure())                   // CHECK-WEAK-LET-NEXT: is nil
  printState(closure())                   // CHECK-WEAK-LET-NEXT: is nil
}

testObjCWeakLetCapture()
#endif
