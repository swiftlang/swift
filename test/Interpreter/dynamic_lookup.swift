// RUN: %target-run-simple-swift | FileCheck %s

import ObjectiveC

class X {
  init() {}

  @objc func f() { println("X.f()") }

  @objc var value : Int {
    println("X.value getter\n")
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
  if of {
    of!()
  } else {
    print("Object does not respond to the selector \"f\".\n")
  }
}

func test_dynamic_lookup_g(obj: AnyObject) {
  var og = obj.dynamicType.g
  if og {
    og!()
  } else {
    print("Class does not respond to the selector \"g\".\n")
  }
}

func test_dynamic_lookup_value(obj: AnyObject) {
  var ov = obj.value
  if ov {
    println("value = \(ov!)")
  } else {
    println("Object does not respond to the selector \"value\".")
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

// CHECK: X.value getter
// CHECK: value = 17
test_dynamic_lookup_value(X())
// CHECK: Object does not respond to the selector "value"
test_dynamic_lookup_value(Y())
