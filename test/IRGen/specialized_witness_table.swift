// RUN: %target-swift-frontend -O -wmo -module-name test -emit-ir %s -o %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck %s --check-prefix=DESC < %t.ll

// In optimized non-embedded Swift, an existential which is created from a specialized
// generic type references a statically emitted specialized witness table instead of
// instantiating the witness table at runtime.

public protocol P {
  func foo() -> Int
}

public protocol Q: P {
  associatedtype A: P
  func bar() -> A
}

public struct Box<T>: P {
  var t: T
  @inline(never)
  public func foo() -> Int { MemoryLayout<T>.size }
}

public struct Wrapper<T>: Q {
  var t: T
  public func foo() -> Int { 1 }
  public func bar() -> Box<T> { Box(t: t) }
}

public struct Cond<T> {
  var t: T
}

extension Cond: P where T: P {
  public func foo() -> Int { t.foo() }
}

// The specialized table has a non-unique definition. Its header is the descriptor of
// the generic conformance and it contains the specialized witness.
// CHECK-DAG: @"$s4test3BoxVySiGAA1PAAWP" = linkonce_odr hidden {{(unnamed_addr )?}}constant {{.*}}@"$s4test3BoxVyxGAA1PAAMc"{{.*}}Si_TG5

// Base protocol and associated conformance entries directly point to specialized tables.
// CHECK-DAG: @"$s4test7WrapperVySbGAA1QAAWP" = linkonce_odr hidden {{.*}}@"$s4test7WrapperVyxGAA1QAAMc"{{.*}}@"$s4test7WrapperVySbGAA1PAAWP"{{.*}}@"$s4test3BoxVySbGAA1PAAWP"
// CHECK-DAG: @"$s4test7WrapperVySbGAA1PAAWP" = linkonce_odr hidden
// CHECK-DAG: @"$s4test3BoxVySbGAA1PAAWP" = linkonce_odr hidden

// CHECK-DAG: @"$s4test4CondVyAA3BoxVySdGGAA1PAAWP" = linkonce_odr hidden {{.*}}@"$s4test4CondVyxGAA1PA2aERzlMc"

// No additional conformance descriptor is emitted for a specialized witness table.
// DESC:     @"$s4test7WrapperVyxGAA1QAAMc" =
// DESC-NOT: @"$s4test7WrapperVyxGAA1QAAMc" =

// CHECK-LABEL: define {{.*}} @"$s4test7makeBoxAA1P_pyF"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK-NOT:     call {{.*}}@"$s4test3BoxVySiGAA1PAAWl"
// CHECK:         store ptr @"$s4test3BoxVySiGAA1PAAWP"
// CHECK:       }
public func makeBox() -> any P {
  return Box(t: 27)
}

// CHECK-LABEL: define {{.*}} @"$s4test11makeWrapperAA1Q_pyF"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test7WrapperVySbGAA1QAAWP"
// CHECK:       }
public func makeWrapper() -> any Q {
  return Wrapper(t: true)
}

// The conditional conformance is not instantiated at runtime.
// CHECK-LABEL: define {{.*}} @"$s4test8makeCondAA1P_pyF"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test4CondVyAA3BoxVySdGGAA1PAAWP"
// CHECK:       }
public func makeCond() -> any P {
  return Cond(t: Box(t: 1.0))
}
