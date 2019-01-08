// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path - %s -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s

// CHECK-LABEL: public enum HasRawValue : Int {
public enum HasRawValue: Int {
  // CHECK-NEXT: case a, b, c
  case a, b = 5, c
  // CHECK-NEXT: public typealias RawValue = Swift.Int
  // CHECK-NEXT: @inlinable public init?(rawValue: Swift.Int)
  // CHECK-NEXT: public var rawValue: Swift.Int {
  // CHECK-NEXT:   @inlinable get{{$}}
  // CHECK-NEXT: }
} // CHECK-NEXT: {{^}$}}

// CHECK-LABEL: @objc public enum ObjCEnum : Int {
@objc public enum ObjCEnum: Int {
  // CHECK-NEXT: case a, b, c
  case a, b = 5, c
  // CHECK-NEXT: public typealias RawValue = Swift.Int
  // CHECK-NEXT: @inlinable public init?(rawValue: Swift.Int)
  // CHECK-NEXT: public var rawValue: Swift.Int {
  // CHECK-NEXT:   @inlinable get{{$}}
  // CHECK-NEXT: }
} // CHECK-NEXT: {{^}$}}
