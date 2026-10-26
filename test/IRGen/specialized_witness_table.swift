// RUN: %target-swift-frontend -O -wmo -module-name test -emit-ir %s -o %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck %s --check-prefix=DESC < %t.ll
// RUN: %FileCheck %s --check-prefix=NOSTATIC < %t.ll

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

// An associated conformance which is the conformance itself.
// CHECK-DAG: @"$s4test4LoopVySiGAA7SelfRefAAWP" = linkonce_odr hidden {{.*}}@"$s4test4LoopVyxGAA7SelfRefAAMc"{{.*}}@"$s4test4LoopVySiGAA7SelfRefAAWP"

// The generic witness of a conditional conformance needs a runtime-instantiated witness table.
// NOSTATIC-NOT: @"$s4test4CondVyAA3BoxVySiGGAA16HasGenericMethodAAWP" =

// A witness which cannot be specialized is the generic witness in the static table.
// CHECK-DAG: @"$s4test4GBoxVySiGAA16HasGenericMethodAAWP" = linkonce_odr hidden {{.*}}@"$s4test4GBoxVyxGAA16HasGenericMethodAAMc"{{.*}}@"$s4test4GBoxVyxGAA16HasGenericMethodA2aEP1gySiqd__lFTW"

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

public protocol HasGenericMethod {
  func g<U>(_ u: U) -> Int
}

public struct GBox<T>: HasGenericMethod {
  var t: T
  public func g<U>(_ u: U) -> Int { MemoryLayout<U>.size }
}

// CHECK-LABEL: define {{.*}} @"$s4test8makeGBoxAA16HasGenericMethod_pyF"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test4GBoxVySiGAA16HasGenericMethodAAWP"
// CHECK:       }
public func makeGBox() -> any HasGenericMethod {
  return GBox(t: 1)
}

// The conditional conformance is not instantiated at runtime.
// CHECK-LABEL: define {{.*}} @"$s4test8makeCondAA1P_pyF"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test4CondVyAA3BoxVySdGGAA1PAAWP"
// CHECK:       }
public func makeCond() -> any P {
  return Cond(t: Box(t: 1.0))
}

public protocol SelfRef {
  associatedtype S: SelfRef
  func next() -> S
}

public struct Loop<T>: SelfRef {
  var t: T
  public func next() -> Loop<T> { self }
}

// CHECK-LABEL: define {{.*}} @"$s4test8makeLoopAA7SelfRef_pyF"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test4LoopVySiGAA7SelfRefAAWP"
// CHECK:       }
public func makeLoop() -> any SelfRef {
  return Loop(t: 1)
}

extension Cond: HasGenericMethod where T: P {
  public func g<U>(_ u: U) -> Int { t.foo() + MemoryLayout<U>.size }
}

// CHECK-LABEL: define {{.*}} @"$s4test15makeCondGenericAA03HasD6Method_pyF"
// CHECK-NOT:     @"$s4test4CondVyAA3BoxVySiGGAA16HasGenericMethodAAWP"
// CHECK:       }
public func makeCondGeneric() -> any HasGenericMethod {
  return Cond(t: Box(t: 1))
}

// A cycle of witness tables, where one table cannot be emitted statically: the generic witness
// of the conditional conformance `B<Int>: Cycle` needs a runtime-instantiated witness table.
// Therefore `A<Int>: Cycle`, which references it, cannot be emitted statically either.
public protocol K {}
extension Int: K {}

public protocol Cycle {
  associatedtype S: Cycle
  func next() -> S
  func g<U>(_ u: U) -> Int
}

public struct A<T: K>: Cycle {
  var t: T
  public func next() -> B<T> { B(t: t) }
  public func g<U>(_ u: U) -> Int { 1 }
}

public struct B<T> {
  var t: T
}

extension B: Cycle where T: K {
  public func next() -> A<T> { A(t: t) }
  public func g<U>(_ u: U) -> Int { 2 }
}

// NOSTATIC-NOT: @"$s4test1AVySiGAA5CycleAAWP" =
// NOSTATIC-NOT: @"$s4test1BVySiGAA5CycleAAWP" =

// CHECK-LABEL: define {{.*}} @"$s4test5makeAAA5Cycle_pyF"
// CHECK-NOT:     @"$s4test1AVySiGAA5CycleAAWP"
// CHECK:       }
public func makeA() -> any Cycle {
  return A(t: 1)
}

// A final class.
public final class FinalBox<T>: Named {
  var t: T
  init(t: T) { self.t = t }
}

public protocol Named {
  func name() -> String
}

extension Named {
  public func name() -> String { "\(type(of: self))" }
}

// CHECK-LABEL: define {{.*}} @"$s4test12makeFinalBoxAA5Named_pyF"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test8FinalBoxCySiGAA5NamedAAWP"
// CHECK:       }
public func makeFinalBox() -> any Named {
  return FinalBox(t: 1)
}
