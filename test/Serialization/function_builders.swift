// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=false -module-to-print=function_builders -I %t -source-filename=%s | %FileCheck %s

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

public protocol ProtocolWithBuilderProperty {
  associatedtype Assoc
  
  // CHECK: @TupleBuilder var myVar: Self.Assoc { get }
  @TupleBuilder var myVar: Assoc { get }
}
