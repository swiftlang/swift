// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name FunctionBuilders -emit-module-interface-path %t/FunctionBuilders.swiftinterface %s
// RUN: %FileCheck %s < %t/FunctionBuilders.swiftinterface
// RUN: %target-swift-frontend -I %t -typecheck -verify %S/Inputs/function_builders_client.swift

@_functionBuilder
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

  public static func buildDo<T>(_ value: T) -> T { return value }
  public static func buildIf<T>(_ value: T?) -> T? { return value }
}

// CHECK-LABEL: public func tuplify<T>(_ cond: Swift.Bool, @FunctionBuilders.TupleBuilder body: (Swift.Bool) -> T)
public func tuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) {
  print(body(cond))
}
