// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

@objc protocol P {
  @objc optional func e()
}

class X {
  init() {}

  @objc func f() { print("X.f()") }

  @objc var myValue : Int {
    print("X.myValue getter\n")
    return 17
  }
}
extension X: P {
  @objc func e() { print("X.e()") }
}

class Y { 
  init() {}
  @objc class func g() { print("Y.g()") }
}
extension Y: P {}

class Z {
   init() {}
}

extension Z { 
  @objc func f() { print("Z.f()") }
}


func test_dynamic_lookup_f(_ obj: AnyObject) {
  var of = obj.f
  if of != nil {
    of!()
  } else {
    print("Object does not respond to the selector \"f\".\n", terminator: "")
  }
}

func test_dynamic_lookup_f_unbound(_ obj: AnyObject) {
  var of = AnyObject.f(obj)
  if of != nil {
    of!()
  } else {
    print("\(type(of: obj)) does not respond to the selector \"f\"")
  }
}

func test_dynamic_lookup_e_unbound(_ obj: AnyObject) {
  var oe = AnyObject.e(obj)
  if oe != nil {
    oe!()
  } else {
    print("\(type(of: obj)) does not respond to the selector \"e\"")
  }
}

func test_dynamic_lookup_g(_ obj: AnyObject) {
  var og = type(of: obj).g
  if og != nil {
    og!()
  } else {
    print("Class does not respond to the selector \"g\".\n", terminator: "")
  }
}

func test_dynamic_lookup_myValue(_ obj: AnyObject) {
  var ov = obj.myValue
  if ov != nil {
    print("myValue = \(ov!)")
  } else {
    print("Object does not respond to the selector \"myValue\".")
  }
}

// CHECK: X.f()
test_dynamic_lookup_f(X())
// CHECK: Object does not respond to the selector "f"
test_dynamic_lookup_f(Y())
// CHECK: Z.f()
test_dynamic_lookup_f(Z())

// CHECK-NEXT: (AnyObject) -> Optional<() -> ()>
print(type(of: AnyObject.f))
// CHECK-NEXT: X.f()
test_dynamic_lookup_f_unbound(X())
// CHECK-NEXT: Y does not respond to the selector "f"
test_dynamic_lookup_f_unbound(Y())
// CHECK-NEXT: Z.f()
test_dynamic_lookup_f_unbound(Z())

// CHECK-NEXT: (AnyObject) -> Optional<() -> ()>
print(type(of: AnyObject.e))
// CHECK-NEXT: X.e()
test_dynamic_lookup_e_unbound(X())
// CHECK-NEXT: Y does not respond to the selector "e"
test_dynamic_lookup_e_unbound(Y())
// CHECK-NEXT: Z does not respond to the selector "e"
test_dynamic_lookup_e_unbound(Z())

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
  override init() { super.init() ; print(#function) }
  deinit { print(#function) }
}

FUNCTION_NAME_TEST()

// CHECK: init()
// CHECK: deinit

