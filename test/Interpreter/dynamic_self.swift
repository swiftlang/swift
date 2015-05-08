// RUN: %target-run-simple-swift | FileCheck %s

// Test IR generation via execution for Self.

protocol P {
  func f() -> Self
  func g() -> Self
}

protocol CP : class {
  func f() -> Self
  func g() -> Self
}

func callDynamicSelfExistential(p: P) {
  print("Before first call")
  var p2 = p.f()
  print("Between calls")
  p2.g()
  print("After second call")
}

func callDynamicSelfClassExistential(cp: CP) {
  print("Before first call")
  var cp2 = cp.f()
  print("Between calls")
  cp2.g()
  print("After second call")
}

struct S : P {
  func f() -> S {
    print("S.f()")
    return self
  }

  func g() -> S {
    print("S.g()")
    return self
  }
}

class C : P, CP {
  init() {
    print("Allocating C")
  }

  deinit {
    print("Destroying C")
  }

  func f() -> Self {
    print("C.f()")
    return self
  }

  func g() -> Self {
    print("C.g()")
    return self
  }
}

print("-------------------------------")

// CHECK: S() as non-class existential
print("S() as non-class existential")
// CHECK-NEXT: Before first call
// CHECK-NEXT: S.f()
// CHECK-NEXT: Between calls
// CHECK-NEXT: S.g()
// CHECK-NEXT: After second call
callDynamicSelfExistential(S())

// CHECK-NEXT: C() as non-class existential
print("C() as non-class existential")
// CHECK-NEXT: Allocating C
// CHECK-NEXT: Before first call
// CHECK-NEXT: C.f()
// CHECK-NEXT: Between calls
// CHECK-NEXT: C.g()
// CHECK-NEXT: After second call
// CHECK-NEXT: Destroying C
callDynamicSelfExistential(C())

// CHECK-NEXT: C() as class existential
print("C() as class existential")
// CHECK-NEXT: Allocating C
// CHECK-NEXT: Before first call
// CHECK-NEXT: C.f()
// CHECK-NEXT: Between calls
// CHECK-NEXT: C.g()
// CHECK-NEXT: After second call
// CHECK-NEXT: Destroying C
callDynamicSelfClassExistential(C())

// CHECK-NEXT: Done
print("Done")
