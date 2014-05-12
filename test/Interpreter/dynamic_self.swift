// RUN: %target-run-simple-swift | FileCheck %s

// Test IR generation via execution for Self.

protocol P {
  func f() -> Self
  func g() -> Self
}

@class_protocol protocol CP {
  func f() -> Self
  func g() -> Self
}

func callDynamicSelfExistential(p: P) {
  println("Before first call")
  var p2 = p.f()
  println("Between calls")
  p2.g()
  println("After second call")
}

func callDynamicSelfClassExistential(cp: CP) {
  println("Before first call")
  var cp2 = cp.f()
  println("Between calls")
  cp2.g()
  println("After second call")
}

struct S : P {
  func f() -> S {
    println("S.f()")
    return self
  }

  func g() -> S {
    println("S.g()")
    return self
  }
}

class C : P, CP {
  init() {
    println("Allocating C")
  }

  deinit {
    println("Destroying C")
  }

  func f() -> Self {
    println("C.f()")
    return self
  }

  func g() -> Self {
    println("C.g()")
    return self
  }
}

println("-------------------------------")

// CHECK: S() as non-class existential
println("S() as non-class existential")
// CHECK-NEXT: Before first call
// CHECK-NEXT: S.f()
// CHECK-NEXT: Between calls
// CHECK-NEXT: S.g()
// CHECK-NEXT: After second call
callDynamicSelfExistential(S())

// CHECK-NEXT: C() as non-class existential
println("C() as non-class existential")
// CHECK-NEXT: Allocating C
// CHECK-NEXT: Before first call
// CHECK-NEXT: C.f()
// CHECK-NEXT: Between calls
// CHECK-NEXT: C.g()
// CHECK-NEXT: After second call
// CHECK-NEXT: Destroying C
callDynamicSelfExistential(C())

// CHECK-NEXT: C() as class existential
println("C() as class existential")
// CHECK-NEXT: Allocating C
// CHECK-NEXT: Before first call
// CHECK-NEXT: C.f()
// CHECK-NEXT: Between calls
// CHECK-NEXT: C.g()
// CHECK-NEXT: After second call
// CHECK-NEXT: Destroying C
callDynamicSelfClassExistential(C())

// CHECK-NEXT: Done
println("Done")
