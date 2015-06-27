// RUN: %target-build-swift %s -Xfrontend -disable-objc-attr-requires-foundation-module -o %t-main
// RUN: %target-run %t-main | FileCheck %s
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

func printState(x : ObjCClassBase?) {
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
