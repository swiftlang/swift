// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/ResultBuilders.swiftinterface) %s -module-name ResultBuilders
// RUN: %FileCheck %s < %t/ResultBuilders.swiftinterface
// RUN: %target-swift-frontend -I %t -typecheck -verify %S/Inputs/result_builders_client.swift
// RUN: %target-swift-frontend -compile-module-from-interface %t/ResultBuilders.swiftinterface -o %t/ResultBuilders.swiftmodule
// RUN: %FileCheck %s < %t/ResultBuilders.swiftinterface

// CHECK: @_functionBuilder public struct TupleBuilder
@resultBuilder
public struct TupleBuilder {
  public static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }

  public static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  public static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  public static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  public static func buildDo<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }

  public static func buildDo<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  public static func buildIf<T>(_ value: T?) -> T? { return value }
}

// CHECK-LABEL: public func tuplify<T>(_ cond: Swift.Bool, @ResultBuilders.TupleBuilder body: (Swift.Bool) -> T)
public func tuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) {
  print(body(cond))
}

public struct UsesBuilderProperty {
  // CHECK: public var myVar: (Swift.String, Swift.String) {
  // CHECK-NEXT: get
  // CHECK-NEXT: }
  @TupleBuilder public var myVar: (String, String) {
    "hello"
    "goodbye"
  }

  // CHECK: public func myFunc(@ResultBuilders.TupleBuilder fn: () -> ())
  public func myFunc(@TupleBuilder fn: () -> ()) {}
}

public protocol ProtocolWithBuilderProperty {
  associatedtype Assoc

  // CHECK: @ResultBuilders.TupleBuilder var myVar: Self.Assoc { get }
  @TupleBuilder var myVar: Assoc { get }

  // CHECK: @ResultBuilders.TupleBuilder func myFunc<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2)
  @TupleBuilder func myFunc<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2)
}
