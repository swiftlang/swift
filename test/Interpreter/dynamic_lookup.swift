// RUN: %swift -i %s | FileCheck %s

class X {
  func [objc] f() { println("X.f()") }
}

class Y { }

func test_dynamic_lookup_f(obj : DynamicLookup) {
  var of = obj.f
  if of {
    of.get()()
  } else {
    print("Object does not respond to the selector \"f\".\n")
  }
}

// CHECK: X.f()
test_dynamic_lookup_f(X())
// CHECK: Object does not respond to the selector "f"
test_dynamic_lookup_f(Y())
