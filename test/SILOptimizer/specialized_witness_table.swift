// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -module-name Resilient -o %t %S/Inputs/specialized_witness_table_resilient.swift
// RUN: %target-swift-frontend -O -wmo -module-name test -I %t -emit-sil %s -o %t/test.sil
// RUN: %FileCheck %s < %t/test.sil
// RUN: %FileCheck %s --check-prefix=NEG < %t/test.sil
// RUN: %target-swift-frontend -Onone -wmo -module-name test -I %t -emit-sil %s | %FileCheck %s --check-prefix=ONONE

// In optimized non-embedded Swift, an existential which is created from a specialized
// generic type gets a specialized witness table with specialized witness methods.
// IRGen emits it statically if possible (see test/IRGen/specialized_witness_table.swift).

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

// A generic method requirement cannot be fully specialized and is kept as the generic witness.
public protocol HasGenericMethod {
  func g<U>(_ u: U) -> Int
}

public struct GBox<T>: HasGenericMethod {
  var t: T
  public func g<U>(_ u: U) -> Int { MemoryLayout<U>.size }
}

// CHECK-LABEL: sil_witness_table shared [specialized] GBox<Int>: specialize <Int> (<T> GBox<T>: HasGenericMethod module test) {
// CHECK-NEXT:    method #HasGenericMethod.g: {{.*}} : @$s4test4GBoxVyxGAA16HasGenericMethodA2aEP1gySiqd__lFTW{{([[:space:]].*)?$}}
// CHECK-NEXT:  }
public func makeGBox() -> any HasGenericMethod {
  return GBox(t: 1)
}

// A generic witness of a conditional conformance would need the conditional conformances,
// which are only stored in a runtime-instantiated witness table. IRGen cannot emit the specialized
// witness table statically.
// CHECK-LABEL: sil_witness_table shared [specialized] Cond<Box<Int>>: specialize <Box<Int>> (<T where T : P> Cond<T>: HasGenericMethod module test) {
// CHECK-NEXT:    method #HasGenericMethod.g: {{.*}} : @$s4test4CondVyxGAA16HasGenericMethodA2A1PRzlAaEP1gySiqd__lFTW{{([[:space:]].*)?$}}
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
