// RUN: %target-swift-frontend -O -wmo -module-name test -emit-ir %s -o %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck %s --check-prefix=DESC < %t.ll
// RUN: %FileCheck %s --check-prefix=NOSTATIC < %t.ll
// RUN: %FileCheck %s --check-prefix=TABLE < %t.ll

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

// A generic witness of a conditional conformance, even if it's partially specialized, needs a
// runtime-instantiated witness table.
// NOSTATIC-NOT: @"$s4test4CondVyAA3BoxVySiGGAA16HasGenericMethodAAWP" =

// The witness of a generic method requirement is partially specialized.
// CHECK-DAG: @"$s4test4GBoxVySiGAA16HasGenericMethodAAWP" = linkonce_odr hidden {{.*}}@"$s4test4GBoxVyxGAA16HasGenericMethodAAMc"{{.*}}@"$s4test4GBoxVyxGAA16HasGenericMethodA2aEP1gySiqd__lFTW{{.*}}_Tp5"

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

// Existentials which are formed in callees of specialized witnesses reference static tables.
public protocol Erased {
  func value() -> Int
}

public struct Inner<T>: Erased {
  var t: T
  public func value() -> Int { MemoryLayout<T>.size }
}

public protocol Outer {
  func erase() -> any Erased
}

public struct OuterBox<T>: Outer {
  var t: T
  @inline(never)
  public func erase() -> any Erased { Inner(t: t) }
}

// TABLE-DAG: @"$s4test5InnerVySiGAA6ErasedAAWP" = linkonce_odr hidden {{.*}}@"$s4test5InnerVyxGAA6ErasedAAMc"{{.*}}Si_TG5"
// TABLE-DAG: @"$s4test8OuterBoxVySiGAA0B0AAWP" = linkonce_odr hidden {{.*}}@"$s4test8OuterBoxVyxGAA0B0AAMc"{{.*}}Si_TG5"

// CHECK-LABEL: define {{.*}} @"$s4test8OuterBoxV5eraseAA6Erased_pyFSi_Tg5"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test5InnerVySiGAA6ErasedAAWP"
// CHECK:       }

// CHECK-LABEL: define {{.*}} @"$s4test9makeOuterAA0C0_pyF"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test8OuterBoxVySiGAA0B0AAWP"
// CHECK:       }
public func makeOuter() -> any Outer {
  return OuterBox(t: 1)
}

public protocol Accessor {
  func access<R>(_ body: (any Erased) -> R) -> R
}

public struct ABox<T>: Accessor {
  var t: T
  @inline(never)
  public func access<R>(_ body: (any Erased) -> R) -> R { body(Inner(t: [t])) }
}

// The static table contains the partially specialized witness.
// TABLE-DAG: @"$s4test4ABoxVySiGAA8AccessorAAWP" = linkonce_odr hidden {{.*}}@"$s4test4ABoxVyxGAA8AccessorAAMc"{{.*}}_Tp5"
// TABLE-DAG: @"$s4test5InnerVySaySiGGAA6ErasedAAWP" = linkonce_odr hidden {{.*}}@"$s4test5InnerVyxGAA6ErasedAAMc"{{.*}}SaySiG_TG5"

// The partial specialization of the callee of the partially specialized witness.
// CHECK-LABEL: define {{.*}} @"$s4test4ABoxV6access{{.*}}_Tp5"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test5InnerVySaySiGGAA6ErasedAAWP"
// CHECK:       }

// CHECK-LABEL: define {{.*}} @"$s4test12makeAccessorAA0C0_pyF"
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         store ptr @"$s4test4ABoxVySiGAA8AccessorAAWP"
// CHECK:       }
public func makeAccessor() -> any Accessor {
  return ABox(t: 1)
}

// A proven cast to a class-bound existential uses the static table.
public protocol PC: AnyObject {
  func foo() -> Int
}

public final class CBox<T>: PC {
  var t: T
  init(t: T) { self.t = t }
  public func foo() -> Int { MemoryLayout<T>.size }
}

@inline(never)
func castToPC<H: AnyObject>(_ h: H) -> any PC {
  return h as! any PC
}

// TABLE-DAG: @"$s4test4CBoxCySiGAA2PCAAWP" = linkonce_odr hidden {{.*}}@"$s4test4CBoxCyxGAA2PCAAMc"{{.*}}Si_Tg5"

// CHECK-LABEL: define {{.*}} @"$s4test8castToPCyAA0D0_pxRlzClFAA4CBoxCySiG_Tg5{{.*}}"
// CHECK-NOT:     call {{.*}}swift_dynamicCast
// CHECK-NOT:     call {{.*}}swift_getWitnessTable
// CHECK:         @"$s4test4CBoxCySiGAA2PCAAWP"
// CHECK:       }
public func makeCastCBox() -> any PC {
  return castToPC(CBox(t: 1))
}
