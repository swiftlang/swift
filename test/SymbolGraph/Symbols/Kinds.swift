// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Kinds -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Kinds -I %t -pretty-print -o %t/Kinds.symbols.json
// RUN: %FileCheck %s --input-file %t/Kinds.symbols.json

// CHECK: "identifier": "swift.class"
// CHECK-NEXT: "displayName": "Class"
public class C {}

// CHECK: "identifier": "swift.struct"
// CHECK-NEXT: "displayName": "Structure"
public struct S {
  // CHECK: "identifier": "swift.variable"
  // CHECK-NEXT: "displayName": "Variable"
  public var x: Int

  // CHECK: "identifier": "swift.initializer"
  // CHECK-NEXT: "displayName": "Initializer"
  public init(x: Int) {
    self.x = x
  }

  // CHECK: "identifier": "swift.function"
  // CHECK-NEXT: "displayName": "Function"
  public func foo() {}
}

// CHECK: "identifier": "swift.enum"
// CHECK-NEXT: "displayName": "Enumeration"
public enum E {
  // CHECK: "identifier": "swift.enum.case"
  // CHECK-NEXT: "displayName": "Case"
  case oneCase
}

// CHECK: "identifier": "swift.protocol"
// CHECK-NEXT: "displayName": "Protocol"
public protocol P {}

// CHECK: "identifier": "swift.typealias"
// CHECK-NEXT: "displayName": "Type Alias"
public typealias Alias = S
