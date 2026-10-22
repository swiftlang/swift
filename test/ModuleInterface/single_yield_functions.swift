// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name SingleYield \
// RUN:   -enable-experimental-feature SingleYieldFunctions -disable-experimental-parser-round-trip
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name SingleYield

// REQUIRES: swift_feature_SingleYieldFunctions

// CHECK: swift-module-flags: {{.*}}-enable-experimental-feature SingleYieldFunctions

public class Box {
  public init() {}
}

// CHECK:      #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT: public func owned(_ b: SingleYield::Box) -> yield SingleYield::Box
// CHECK-NEXT: #endif
public func owned(_ b: Box) -> yield Box { yield b }

// CHECK:      #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT: public func borrowed(_ x: Swift::Int) -> yield borrow Swift::Int
// CHECK-NEXT: #endif
public func borrowed(_ x: Int) -> yield borrow Int { yield x }

// CHECK:      #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT: public func mutable(_ x: inout Swift::Int) -> yield inout Swift::Int
// CHECK-NEXT: #endif
public func mutable(_ x: inout Int) -> yield inout Int { yield &x }

// CHECK:      #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT: public func generic<T>(_ x: T) -> yield borrow T
// CHECK-NEXT: #endif
public func generic<T>(_ x: T) -> yield borrow T { yield x }

// CHECK:      public struct S<T> {
// CHECK:        #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT:   public func borrowed() -> yield borrow T
// CHECK-NEXT:   #endif
// CHECK-NEXT:   #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT:   public mutating func mutable() -> yield inout T
// CHECK-NEXT:   #endif
// CHECK-NEXT:   #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT:   public static func zero() -> yield Swift::Int
// CHECK-NEXT:   #endif
// CHECK-NEXT: }
public struct S<T> {
  public var value: T

  public func borrowed() -> yield borrow T { yield value }
  public mutating func mutable() -> yield inout T { yield &value }
  public static func zero() -> yield Int { yield 0 }
}

// CHECK:      public protocol P {
// CHECK-NEXT:   #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT:   func borrowed() -> yield borrow Swift::Int
// CHECK-NEXT:   #endif
// CHECK-NEXT:   #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT:   mutating func mutable() -> yield inout Swift::Int
// CHECK-NEXT:   #endif
// CHECK-NEXT:   #if compiler(>=5.3) && $SingleYieldFunctions
// CHECK-NEXT:   func generic<U>(_ x: U) -> yield U
// CHECK-NEXT:   #endif
// CHECK-NEXT: }
public protocol P {
  func borrowed() -> yield borrow Int
  mutating func mutable() -> yield inout Int
  func generic<U>(_ x: U) -> yield U
}
