// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name synthesized -disable-objc-attr-requires-foundation-module -enable-objc-interop
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name synthesized
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.swiftinterface

// CHECK-LABEL: public enum HasRawValue : Swift.Int {
public enum HasRawValue: Int {
  // CHECK-NEXT: case a, b, c
  case a, b = 5, c
  // CHECK-DAG: public typealias RawValue = Swift.Int
  // CHECK-DAG: public init?(rawValue: Swift.Int)
  // CHECK-DAG: public var rawValue: Swift.Int {
  // CHECK-DAG:   get{{$}}
  // CHECK-DAG: }
} // CHECK: {{^}$}}

// CHECK-LABEL: @available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
// CHECK-NEXT: public enum HasRawValueAndAvailability : Swift.Int {
@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
public enum HasRawValueAndAvailability: Int {
  // CHECK-NEXT: case a, b, c
  case a, b, c
  // CHECK-DAG: public typealias RawValue = Swift.Int
  // CHECK-DAG: public init?(rawValue: Swift.Int)
  // CHECK-DAG: public var rawValue: Swift.Int {
  // CHECK-DAG:   get{{$}}
  // CHECK-DAG: }
} // CHECK: {{^}$}}

@objc public enum ObjCEnum: Int32 {
  case a, b = 5, c
}

// CHECK-LABEL: @objc public enum ObjCEnum : Swift.Int32 {
// CHECK-NEXT: case a, b = 5, c
// CHECK-DAG: public typealias RawValue = Swift.Int32
// CHECK-DAG: public init?(rawValue: Swift.Int32)
// CHECK-DAG: public var rawValue: Swift.Int32 {
// CHECK-DAG:   get{{$}}
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

// CHECK-LABEL: extension synthesized.NoRawValueWithExplicitHashable : Swift.Hashable {
extension NoRawValueWithExplicitHashable : Hashable {
  // CHECK-NEXT: public func foo()
  public func foo() {}
} // CHECK: {{^}$}}

// CHECK: extension synthesized.HasRawValue : Swift.Equatable {}
// CHECK: extension synthesized.HasRawValue : Swift.Hashable {}
// CHECK: extension synthesized.HasRawValue : Swift.RawRepresentable {}

// CHECK: @available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
// CHECK-NEXT: extension synthesized.HasRawValueAndAvailability : Swift.Equatable {}
// CHECK: @available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
// CHECK-NEXT: extension synthesized.HasRawValueAndAvailability : Swift.Hashable {}
// CHECK: @available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
// CHECK-NEXT: extension synthesized.HasRawValueAndAvailability : Swift.RawRepresentable {}

// CHECK: extension synthesized.ObjCEnum : Swift.Equatable {}
// CHECK: extension synthesized.ObjCEnum : Swift.Hashable {}
// CHECK: extension synthesized.ObjCEnum : Swift.RawRepresentable {}

// CHECK: extension synthesized.NoRawValueWithExplicitEquatable : Swift.Hashable {}
// NEGATIVE-NOT: extension {{.+}}NoRawValueWithExplicitEquatable : Swift.Equatable

// NEGATIVE-NOT: NoRawValueWithExplicitHashable : Swift.Equatable
// NEGATIVE-NOT: NoRawValueWithExplicitHashable : Swift.Hashable {}
