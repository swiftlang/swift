// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/MyModule.swiftinterface -enable-library-evolution %s -module-name MyModule
// RUN: %FileCheck %s < %t/MyModule.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface %t/MyModule.swiftinterface -o %t/MyModule.swiftmodule

// CHECK: public struct Type {
// CHECK-NEXT: }
public struct Type {}

// CHECK: public protocol Protocol {
// CHECK-NEXT: }
public protocol Protocol {}

// CHECK: public func usesType(_ x: MyModule.`Type`)
public func usesType(_ x: Type) {}

// CHECK: public func genericProtocol<T>(_ x: T) where T : MyModule.`Protocol`
public func genericProtocol<T: Protocol>(_ x: T) {}

// CHECK: public func existentialProtocol(_ x: MyModule.`Protocol`)
public func existentialProtocol(_ x: Protocol) {}

// CHECK: public struct Parent {
public struct Parent {
  // CHECK: public struct `Type` {
  public struct `Type` {
    // CHECK: public struct `Protocol` {
    // CHECK-NEXT: }
    public struct `Protocol` {}
    // CHECK-NEXT: }
  }
  // CHECK: public struct `Protocol` {
  // CHECK-NEXT: }
  public struct `Protocol` {}
  // CHECK-NEXT: }
}

// CHECK: public func usesNestedType(_ x: MyModule.Parent.`Type`)
public func usesNestedType(_ x: Parent.`Type`) {}

// CHECK: public func usesNestedTypeProtocol(_ x: MyModule.Parent.`Type`.`Protocol`)
public func usesNestedTypeProtocol(_ x: Parent.`Type`.`Protocol`) {}

// CHECK: public func usesNestedProtocol(_ x: MyModule.Parent.`Protocol`)
public func usesNestedProtocol(_ x: Parent.`Protocol`) {}
