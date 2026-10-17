// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -module-name Resilient -o %t %S/Inputs/specialized_witness_table_resilient.swift
// RUN: %target-swift-frontend -O -wmo -module-name test -I %t -emit-sil %s -o %t/test.sil
// RUN: %FileCheck %s < %t/test.sil
// RUN: %FileCheck %s --check-prefix=NEG < %t/test.sil
// RUN: %target-swift-frontend -Onone -wmo -module-name test -I %t -emit-sil %s | %FileCheck %s --check-prefix=ONONE

// In optimized non-embedded Swift, an existential which is created from a specialized
// generic type gets a statically emitted specialized witness table with specialized
// witness methods.

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

// A conformance to a resilient protocol has no fixed witness table layout.
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

// Negative cases: no specialized witness tables for the following.

// A default implementation needs the dynamic Self type of a non-final class, which can be
// a sub-class. Such a witness has a generic Self and is not specialized for the class type.
public protocol Named {
  func name() -> String
}

extension Named {
  public func name() -> String { "\(type(of: self))" }
}

public func makeNamed() -> any Named {
  return Base<Int>()
}

// A generic requirement cannot be fully specialized.
public protocol HasGenericMethod {
  func g<U>(_ u: U) -> Int
}

public struct GBox<T>: HasGenericMethod {
  var t: T
  public func g<U>(_ u: U) -> Int { MemoryLayout<U>.size }
}

public func makeGBox() -> any HasGenericMethod {
  return GBox(t: 1)
}

// NEG-NOT: [specialized] RBox
// NEG-NOT: [specialized] Base<Int>: specialize <Int> (<T> Base<T>: Named
// NEG-NOT: [specialized] GBox
