// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -module-name Resilient -o %t %S/Inputs/specialized_witness_table_resilient.swift
// RUN: %target-swift-frontend -O -wmo -module-name test -I %t -emit-sil %s -o %t/test.sil
// RUN: %FileCheck %s < %t/test.sil
// RUN: %FileCheck %s --check-prefix=NEG < %t/test.sil
// RUN: %FileCheck %s --check-prefix=SIG < %t/test.sil
// RUN: %target-swift-frontend -Onone -wmo -module-name test -I %t -emit-sil %s | %FileCheck %s --check-prefix=ONONE
// RUN: %target-swift-frontend -Osize -wmo -module-name test -I %t -emit-sil %s | %FileCheck %s --check-prefix=ONONE

// In optimized non-embedded Swift, an existential which is created from a specialized
// generic type gets a specialized witness table with specialized witness methods.
// IRGen emits it statically if possible (see test/IRGen/specialized_witness_table.swift).

// Specialized witness tables are not created without optimization and when optimizing for size,
// because all witness methods are specialized, even if they are never called.
// ONONE-NOT: [specialized]

import Resilient

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

// Returning the existentials prevents them from being optimized away.

// CHECK-LABEL: sil_witness_table shared [specialized] Box<Int>: specialize <Int> (<T> Box<T>: P module test) {
// CHECK-NEXT:    method #P.foo: {{.*}} : @$s4test3BoxVyxGAA1PA2aEP3fooSiyFTW{{.*}}Si_TG5
// CHECK-NEXT:  }
public func makeBox() -> any P {
  return Box(t: 27)
}

// CHECK-LABEL: sil_witness_table shared [specialized] Wrapper<Bool>: specialize <Bool> (<T> Wrapper<T>: Q module test) {
// CHECK-DAG:     base_protocol P: Wrapper<Bool>: specialize <Bool> (<T> Wrapper<T>: P module test)
// CHECK-DAG:     associated_conformance (A: P): Box<Bool>: specialize <Bool> (<T> Box<T>: P module test)
// CHECK-DAG:     associated_type A: Box<Bool>
// CHECK-DAG:     method #Q.bar: {{.*}} : @$s4test7WrapperVyxGAA1QA2aEP3bar1AQzyFTW{{.*}}Sb_TG5
// CHECK:       }
public func makeWrapper() -> any Q {
  return Wrapper(t: true)
}

// CHECK-LABEL: sil_witness_table shared [specialized] Cond<Box<Double>>: specialize <Box<Double>> (<T where T : P> Cond<T>: P module test) {
// CHECK-NEXT:    method #P.foo: {{.*}} : @$s4test4CondVyxGAA1PA2aERzlAaEP3fooSiyFTW{{.*}}_TG5
// CHECK-NEXT:  }
public func makeCond() -> any P {
  return Cond(t: Box(t: 1.0))
}

// A conformance to a resilient protocol has no fixed witness table layout. IRGen cannot emit the
// specialized witness table statically, but it's still available for optimizations.
// CHECK-LABEL: sil_witness_table shared [specialized] RBox<Int>: specialize <Int> (<T> RBox<T>: ResilientP module test) {
// CHECK-NEXT:    method #ResilientP.foo: {{.*}} : @$s4test4RBoxVyxG9Resilient0C1PAaeFP3fooSiyFTWSi_TG5
// CHECK-NEXT:  }
public struct RBox<T>: ResilientP {
  var t: T
  public func foo() -> Int { 2 }
}

public func makeResilient() -> any ResilientP {
  return RBox(t: 1)
}

// The witness of a non-final class calls the method through the vtable. Therefore
// the specialized witness also works for sub-classes.
public protocol Copies {
  func copy() -> Self
}

open class Base<T>: Copies, Named {
  public required init() {}
  public func copy() -> Self { Self() }
}

// CHECK-LABEL: sil_witness_table shared [specialized] Base<Int>: specialize <Int> (<T> Base<T>: Copies module test) {
// CHECK-NEXT:    method #Copies.copy: {{.*}} : @$s4test4BaseCyxGAA6CopiesA2aEP4copyxyFTWSi_TG5
// CHECK-NEXT:  }
public func makeBase() -> any Copies {
  return Base<Int>()
}

// Witnesses which cannot be specialized are kept as generic witnesses. They get the generic
// parameters of the conformance from the type metadata of Self.

// A default implementation needs the dynamic Self type of a non-final class, which can be
// a sub-class. Such a witness has a generic Self and is not specialized for the class type.
public protocol Named {
  func name() -> String
}

extension Named {
  public func name() -> String { "\(type(of: self))" }
}

// CHECK-LABEL: sil_witness_table shared [specialized] Base<Int>: specialize <Int> (<T> Base<T>: Named module test) {
// CHECK-NEXT:    method #Named.name: {{.*}} : @$s4test4BaseCyqd__GAA5NamedA2aEP4nameSSyFTW{{([[:space:]].*)?$}}
// CHECK-NEXT:  }
public func makeNamed() -> any Named {
  return Base<Int>()
}

// For a final class, the default implementation is specialized for the concrete self type.
public final class FinalBase<T>: Named {
  public init() {}
}

// CHECK-LABEL: sil_witness_table shared [specialized] FinalBase<Int>: specialize <Int> (<T> FinalBase<T>: Named module test) {
// CHECK-NEXT:    method #Named.name: {{.*}} : @$s4test9FinalBaseCyxGAA5NamedA2aEP4nameSSyFTW{{.*}}_TG5
// CHECK-NEXT:  }
public func makeFinalBase() -> any Named {
  return FinalBase<Int>()
}

// For a struct, the default implementation is specialized for the concrete self type.
public struct SBox<T>: Named {
  var t: T
}

// CHECK-LABEL: sil_witness_table shared [specialized] SBox<Int>: specialize <Int> (<T> SBox<T>: Named module test) {
// CHECK-NEXT:    method #Named.name: {{.*}} : @$s4test4SBoxVyxGAA5NamedA2aEP4nameSSyFTW{{.*}}_TG5
// CHECK-NEXT:  }
public func makeSBox() -> any Named {
  return SBox(t: 1)
}

// The witness of a generic method requirement is partially specialized: the generic parameters
// of the conformance become concrete, the generic parameters of the requirement stay generic.
public protocol HasGenericMethod {
  func g<U>(_ u: U) -> Int
}

public struct GBox<T>: HasGenericMethod {
  var t: T
  public func g<U>(_ u: U) -> Int { MemoryLayout<U>.size }
}

// The partially specialized witness has the same generic parameters as the generic witness.
// SIG-LABEL: sil shared [transparent] [thunk] @$s4test4GBoxVyxGAA16HasGenericMethodA2aEP1gySiqd__lFTW{{.*}}_Tp5 : $@convention(witness_method: HasGenericMethod) <τ_0_0 where τ_0_0 == Int><τ_1_0> (@in_guaranteed τ_1_0, @in_guaranteed GBox<Int>) -> Int {

// CHECK-LABEL: sil_witness_table shared [specialized] GBox<Int>: specialize <Int> (<T> GBox<T>: HasGenericMethod module test) {
// CHECK-NEXT:    method #HasGenericMethod.g: {{.*}} : @$s4test4GBoxVyxGAA16HasGenericMethodA2aEP1gySiqd__lFTW{{.*}}_Tp5
// CHECK-NEXT:  }
public func makeGBox() -> any HasGenericMethod {
  return GBox(t: 1)
}

// The witness of a generic method requirement in a conditional conformance is partially specialized, too.
// CHECK-LABEL: sil_witness_table shared [specialized] Cond<Box<Int>>: specialize <Box<Int>> (<T where T : P> Cond<T>: HasGenericMethod module test) {
// CHECK-NEXT:    method #HasGenericMethod.g: {{.*}} : @$s4test4CondVyxGAA16HasGenericMethodA2A1PRzlAaEP1gySiqd__lFTW{{.*}}_Tp5
// CHECK-NEXT:  }
extension Cond: HasGenericMethod where T: P {
  public func g<U>(_ u: U) -> Int { t.foo() + MemoryLayout<U>.size }
}

public func makeCondGeneric() -> any HasGenericMethod {
  return Cond(t: Box(t: 1))
}

// The witness table of a non-generic conformance is not specialized, even if it has a default
// method, because IRGen emits the original witness table.
public protocol D {
  func d() -> Int
}

extension D {
  public func d() -> Int { 3 }
}

public struct Plain: D {}

public protocol HasD {
  associatedtype X: D
  func x() -> X
}

public struct HBox<T>: HasD {
  var t: T
  public func x() -> Plain { Plain() }
}

// CHECK-LABEL: sil_witness_table shared [specialized] HBox<Int>: specialize <Int> (<T> HBox<T>: HasD module test) {
// CHECK:         associated_conformance (X: D): Plain: D module test
// CHECK:       }
public func makeHBox() -> any HasD {
  return HBox(t: 1)
}

// NEG-NOT: [specialized] Plain: D

// The witness tables of existentials which are formed in callees of specialized witnesses
// are specialized, too. E.g. a type which erases itself into another protocol.
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

// CHECK-LABEL: sil_witness_table shared [specialized] OuterBox<Int>: specialize <Int> (<T> OuterBox<T>: Outer module test) {
// CHECK-NEXT:    method #Outer.erase: {{.*}} : @$s4test8OuterBoxVyxGAA0B0A2aEP5eraseAA6Erased_pyFTWSi_TG5
// CHECK-NEXT:  }
public func makeOuter() -> any Outer {
  return OuterBox(t: 1)
}

// An existential which is formed in the callee of a partially specialized witness.
public protocol Accessor {
  func access<R>(_ body: (any Erased) -> R) -> R
}

public struct ABox<T>: Accessor {
  var t: T
  @inline(never)
  public func access<R>(_ body: (any Erased) -> R) -> R { body(Inner(t: [t])) }
}

// SIG-LABEL: sil shared [transparent] [thunk] @$s4test4ABoxVyxGAA8AccessorA2aEP6access{{.*}}_Tp5 : $@convention(witness_method: Accessor) <τ_0_0 where τ_0_0 == Int><τ_1_0> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed any Erased) -> @out τ_0_0 for <τ_1_0>, @in_guaranteed ABox<Int>) -> @out τ_1_0 {

// CHECK-LABEL: sil_witness_table shared [specialized] ABox<Int>: specialize <Int> (<T> ABox<T>: Accessor module test) {
// CHECK-NEXT:    method #Accessor.access: {{.*}} : @$s4test4ABoxVyxGAA8AccessorA2aEP6access{{.*}}_Tp5
// CHECK-NEXT:  }
public func makeAccessor() -> any Accessor {
  return ABox(t: 1)
}

// A generic requirement with typed throws.
public protocol ThrowingAccessor {
  func access<R, E: Error>(_ body: (Int) throws(E) -> R) throws(E) -> R
}

public struct TBox<T>: ThrowingAccessor {
  var t: T
  public func access<R, E: Error>(_ body: (Int) throws(E) -> R) throws(E) -> R {
    try body(MemoryLayout<T>.size)
  }
}

// SIG-LABEL: sil shared [transparent] [thunk] @$s4test4TBoxVyxGAA16ThrowingAccessorA2aEP6access{{.*}}_Tp5 : $@convention(witness_method: ThrowingAccessor) <τ_0_0 where τ_0_0 == Double><τ_1_0, τ_1_1 where τ_1_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> (Int) -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_1_1, τ_1_0>, @in_guaranteed TBox<Double>) -> (@out τ_1_0, @error_indirect τ_1_1) {

// CHECK-LABEL: sil_witness_table shared [specialized] TBox<Double>: specialize <Double> (<T> TBox<T>: ThrowingAccessor module test) {
// CHECK-NEXT:    method #ThrowingAccessor.access: {{.*}} : @$s4test4TBoxVyxGAA16ThrowingAccessorA2aEP6access{{.*}}_Tp5
// CHECK-NEXT:  }
public func makeThrowingAccessor() -> any ThrowingAccessor {
  return TBox(t: 1.0)
}

// A cast of a specialized class to a class-bound existential, which is proven to succeed,
// becomes an init_existential_ref, which gets a specialized witness table.
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

// CHECK-LABEL: sil_witness_table shared [specialized] CBox<Int>: specialize <Int> (<T> CBox<T>: PC module test) {
// CHECK-NEXT:    method #PC.foo: {{.*}} : @$s4test4CBoxCyxGAA2PCA2aEP3fooSiyFTWSi_Tg5
// CHECK-NEXT:  }
public func makeCastCBox() -> any PC {
  return castToPC(CBox(t: 1))
}

// The witness tables of the existentials in the callees of the specialized witnesses
// of `ABox<Int>: Accessor` and `OuterBox<Int>: Outer`.
// CHECK-LABEL: sil_witness_table shared [specialized] Inner<Array<Int>>: specialize <Array<Int>> (<T> Inner<T>: Erased module test) {
// CHECK-NEXT:    method #Erased.value: {{.*}} : @$s4test5InnerVyxGAA6ErasedA2aEP5valueSiyFTWSaySiG_TG5
// CHECK-NEXT:  }
// CHECK-LABEL: sil_witness_table shared [specialized] Inner<Int>: specialize <Int> (<T> Inner<T>: Erased module test) {
// CHECK-NEXT:    method #Erased.value: {{.*}} : @$s4test5InnerVyxGAA6ErasedA2aEP5valueSiyFTWSi_TG5
// CHECK-NEXT:  }
