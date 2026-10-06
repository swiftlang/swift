// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/DeinitableLib.swiftinterface) %s -enable-experimental-feature NondeinitableTypes -module-name DeinitableLib
// RUN: %target-swift-typecheck-module-from-interface(%t/DeinitableLib.swiftinterface) -module-name DeinitableLib
// RUN: %FileCheck %s < %t/DeinitableLib.swiftinterface

// REQUIRES: swift_feature_NondeinitableTypes

// CHECK:      #if compiler(>=5.3) && $NondeinitableTypes
// CHECK-NEXT: @frozen public struct ND : ~Swift::Copyable, ~Swift::Deinitable {
@frozen
public struct ND: ~Copyable, ~Deinitable {
  public init() {}

  public consuming func finish() {
    discard self
  }
}

// CHECK:      #if compiler(>=5.3) && $NondeinitableTypes
// CHECK-NEXT: public func borrowNondeinitable<T>(_ t: borrowing T) where T : ~Copyable, T : ~Deinitable
public func borrowNondeinitable<T: ~Copyable & ~Deinitable>(_ t: borrowing T) {}

// CHECK:      public func borrowNoncopyable<T>(_ t: borrowing T) where T : ~Copyable
public func borrowNoncopyable<T: ~Copyable>(_ t: borrowing T) {}

// CHECK:      #if compiler(>=5.3) && $NondeinitableTypes
// CHECK-NEXT: public protocol HasNondeinitable {
// CHECK-NEXT:   associatedtype A : ~Copyable, ~Deinitable
// CHECK-NEXT: }
// CHECK-NEXT: #endif
public protocol HasNondeinitable {
  associatedtype A: ~Copyable, ~Deinitable
}

// CHECK:      public protocol HasNoncopyable {
// CHECK-NEXT:   associatedtype A : ~Copyable
// CHECK-NEXT: }
public protocol HasNoncopyable {
  associatedtype A: ~Copyable
}

// The implicit `Deinitable` requirements of `~Copyable` declarations don't
// appear in the interface.

// CHECK-NOT:  #if
// CHECK:      public protocol NoncopyableProto : ~Copyable {
// CHECK-NEXT: }
public protocol NoncopyableProto: ~Copyable {}

// CHECK-NOT:  #if
// CHECK:      public struct NoncopyableBox<T> : ~Swift::Copyable where T : ~Copyable {
public struct NoncopyableBox<T: ~Copyable>: ~Copyable {}

// CHECK-NOT:  #if
// CHECK:      extension DeinitableLib::NoncopyableBox : Swift::Copyable where T : Swift::Copyable {
extension NoncopyableBox: Copyable where T: Copyable {}

// CHECK-NOT:  #if
// CHECK:      public func borrowSomeNoncopyable(_: borrowing some ~Copyable)
public func borrowSomeNoncopyable(_: borrowing some ~Copyable) {}

// CHECK:      #if compiler(>=5.3) && $NondeinitableTypes
// CHECK-NEXT: public struct Box<T> : ~Swift::Copyable, ~Swift::Deinitable where T : ~Copyable, T : ~Deinitable {
public struct Box<T: ~Copyable & ~Deinitable>: ~Copyable, ~Deinitable {
  public var value: T
}

// The implicit `Sendable` conformance of a `~Deinitable` type needs the
// feature, too.
// CHECK:      #if compiler(>=5.3) && $NondeinitableTypes
// CHECK-NEXT: extension DeinitableLib::ND : Swift::Sendable {}
// CHECK-NEXT: #endif
