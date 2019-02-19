// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path - %s -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s

// CHECK-LABEL: public enum HasRawValue : Int {
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

// CHECK-LABEL: @objc public enum ObjCEnum : Int32 {
// CHECK-NEXT: case a, b, c
// CHECK-DAG: public typealias RawValue = Swift.Int32
// CHECK-DAG: @inlinable public init?(rawValue: Swift.Int32)
// CHECK-DAG: public var rawValue: Swift.Int32 {
// CHECK-DAG:   @inlinable get{{$}}
// CHECK-DAG: }
// CHECK: {{^}$}}
