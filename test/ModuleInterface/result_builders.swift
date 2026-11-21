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

// CHECK-LABEL: public func tuplify<T>(_ cond: Swift::Bool, @ResultBuilders::TupleBuilder body: (Swift::Bool) -> T)
public func tuplify<T>(_ cond: Bool, @TupleBuilder body: (Bool) -> T) {
  print(body(cond))
}

public struct UsesBuilderProperty {
  // CHECK: public var myVar: (Swift::String, Swift::String) {
  // CHECK-NEXT: get
  // CHECK-NEXT: }
  @TupleBuilder public var myVar: (String, String) {
    "hello"
    "goodbye"
  }

  // CHECK: public func myFunc(@ResultBuilders::TupleBuilder fn: () -> ())
  public func myFunc(@TupleBuilder fn: () -> ()) {}
}

public protocol ProtocolWithBuilderProperty {
  associatedtype Assoc

  // CHECK: @ResultBuilders::TupleBuilder var myVar: Self.Assoc { get }
  @TupleBuilder var myVar: Assoc { get }

  // CHECK: @ResultBuilders::TupleBuilder func myFunc<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2)
  @TupleBuilder func myFunc<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2)
}

// Generic arguments inferred for a result builder attribute are printed
// explicitly, so the interface can be used by compilers that don't support
// inferring them.
@resultBuilder
public enum ArrayBuilder<Element> {
  public static func buildBlock(_ elements: Element...) -> [Element] {
    elements
  }
}

@resultBuilder
public enum DictionaryBuilder<Key: Hashable, Value> {
  public static func buildBlock(_ elements: (Key, Value)...) -> [Key: Value] {
    Dictionary(elements, uniquingKeysWith: { $1 })
  }
}

// CHECK-LABEL: public func makeArray(@ResultBuilders::ArrayBuilder<Swift::String> _ build: () -> [Swift::String]) -> [Swift::String]
public func makeArray(@ArrayBuilder _ build: () -> [String]) -> [String] {
  build()
}

// CHECK-LABEL: public func makeDictionary(@ResultBuilders::DictionaryBuilder<Swift::String, Swift::Int> _ build: () -> [Swift::String : Swift::Int]) -> [Swift::String : Swift::Int]
public func makeDictionary(
  @DictionaryBuilder<String, _> _ build: () -> [String: Int]
) -> [String: Int] {
  build()
}

// CHECK-LABEL: public func makeGenericArray<Element>(@ResultBuilders::ArrayBuilder<Element> _ build: () -> [Element]) -> [Element]
public func makeGenericArray<Element>(
  @ArrayBuilder _ build: () -> [Element]
) -> [Element] {
  build()
}

public protocol ProtocolWithInferredBuilder {
  // CHECK: @ResultBuilders::ArrayBuilder<Swift::String> var items: [Swift::String] { get }
  @ArrayBuilder var items: [String] { get }
}
