// Enum case raw value expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -emit-module -module-name EnumRawValueInterface -emit-module-interface-path %t/EnumRawValueInterface.swiftinterface -enable-library-evolution -swift-version 5 %s -enable-experimental-feature LiteralExpressions
// RUN: %FileCheck %s < %t/EnumRawValueInterface.swiftinterface

// CHECK-LABEL: public enum E1 : Swift::Int {
public enum E1: Int {
    // CHECK-NEXT: case a
    // CHECK-NEXT: case b
    // CHECK-NEXT: case c
    case a = 2 + 2
    case b
    case c
  // CHECK-DAG: public typealias RawValue = Swift::Int
  // CHECK-DAG: public init?(rawValue: Swift::Int)
  // CHECK-DAG: public var rawValue: Swift::Int {
  // CHECK-DAG:   get{{$}}
  // CHECK-DAG: }    
} // CHECK: {{^}$}}
