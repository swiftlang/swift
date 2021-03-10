// RUN: %target-swift-frontend -typecheck -emit-module-interface-path - %s -disable-objc-attr-requires-foundation-module -enable-objc-interop > %t.swiftinterface
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.swiftinterface

// CHECK-LABEL: public enum HasRawValue : Swift.Int {
public enum HasRawValue: Int {
  // CHECK-NEXT: case a, b, c
  case a, b = 5, c
  // CHECK-DAG: public typealias RawValue = Swift.Int
  // CHECK-DAG: @inlinable public init?(rawValue: Swift.Int)
  // CHECK-DAG: public var rawValue: Swift.Int {
  // CHECK-DAG:   @inlinable get{{$}}
  // CHECK-DAG: }
} // CHECK: {{^}$}}

@objc public enum ObjCEnum: Int32 {
  case a, b = 5, c
}

// CHECK-LABEL: @objc public enum ObjCEnum : Swift.Int32 {
// CHECK-NEXT: case a, b = 5, c
// CHECK-DAG: public typealias RawValue = Swift.Int32
// CHECK-DAG: @inlinable public init?(rawValue: Swift.Int32)
// CHECK-DAG: public var rawValue: Swift.Int32 {
// CHECK-DAG:   @inlinable get{{$}}
// CHECK-DAG: }
// CHECK: {{^}$}}

// CHECK-LABEL: public enum NoRawValueWithExplicitEquatable : Swift.Equatable {
public enum NoRawValueWithExplicitEquatable : Equatable {
  // CHECK-NEXT: case a, b, c
  case a, b, c
} // CHECK: {{^}$}}

// CHECK-LABEL: public enum NoRawValueWithExplicitHashable {
public enum NoRawValueWithExplicitHashable {
  // CHECK-NEXT: case a, b, c
case a, b, c
} // CHECK: {{^}$}}

// CHECK-LABEL: extension NoRawValueWithExplicitHashable : Swift.Hashable {
extension NoRawValueWithExplicitHashable : Hashable {
  // CHECK-NEXT: public func foo()
  public func foo() {}
} // CHECK: {{^}$}}

// CHECK: extension HasRawValue : Swift.Equatable {}
// CHECK: extension HasRawValue : Swift.Hashable {}
// CHECK: extension HasRawValue : Swift.RawRepresentable {}

// CHECK: extension ObjCEnum : Swift.Equatable {}
// CHECK: extension ObjCEnum : Swift.Hashable {}
// CHECK: extension ObjCEnum : Swift.RawRepresentable {}

// CHECK: extension NoRawValueWithExplicitEquatable : Swift.Hashable {}
// NEGATIVE-NOT: extension {{.+}}NoRawValueWithExplicitEquatable : Swift.Equatable

// NEGATIVE-NOT: NoRawValueWithExplicitHashable : Swift.Equatable
// NEGATIVE-NOT: NoRawValueWithExplicitHashable : Swift.Hashable {}
