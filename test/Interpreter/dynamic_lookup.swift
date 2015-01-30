// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

class X {
  init() {}

  @objc func f() { println("X.f()") }

  @objc var myValue : Int {
    println("X.myValue getter\n")
    return 17
  }
}

class Y { 
  init() {}
  @objc class func g() { println("Y.g()") }
}

class Z {
   init() {}
}

extension Z { 
  @objc func f() { println("Z.f()") }
}


func test_dynamic_lookup_f(obj: AnyObject) {
  var of = obj.f
  if of != nil {
    of!()
  } else {
    print("Object does not respond to the selector \"f\".\n")
  }
}

func test_dynamic_lookup_g(obj: AnyObject) {
  var og = obj.dynamicType.g
  if og != nil {
    og!()
  } else {
    print("Class does not respond to the selector \"g\".\n")
  }
}

func test_dynamic_lookup_myValue(obj: AnyObject) {
  var ov = obj.myValue
  if ov != nil {
    println("myValue = \(ov!)")
  } else {
    println("Object does not respond to the selector \"myValue\".")
  }
}

// CHECK: X.f()
test_dynamic_lookup_f(X())
// CHECK: Object does not respond to the selector "f"
test_dynamic_lookup_f(Y())
// CHECK: Z.f()
test_dynamic_lookup_f(Z())

// CHECK: Class does not respond to the selector "g"
test_dynamic_lookup_g(X())
// CHECK: Y.g()
test_dynamic_lookup_g(Y())

// CHECK: X.myValue getter
// CHECK: myValue = 17
test_dynamic_lookup_myValue(X())
// CHECK: Object does not respond to the selector "myValue"
test_dynamic_lookup_myValue(Y())


// <rdar://problem/16554056> __FUNCTION__ in deinit for NSObject subclasses crashes the compiler
// Test __FUNCTION__
class FUNCTION_NAME_TEST : NSObject {
  override init() { super.init() ; println(__FUNCTION__) }
  deinit { println(__FUNCTION__) }
}

FUNCTION_NAME_TEST()

// CHECK: init()
// CHECK: deinit

