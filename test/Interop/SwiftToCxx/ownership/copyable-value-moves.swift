// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Moves -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %FileCheck %s < %t/moves.h
// RUN: %check-interop-cxx-header-in-clang(%t/moves.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public final class Ref {
  public init() {}
}

public struct Value {
  public var ref: Ref
  public weak var weakRef: Ref?
  public var number: Int

  public init(_ ref: Ref, _ number: Int) {
    self.ref = ref
    self.weakRef = ref
    self.number = number
  }

  public func check() -> Bool { weakRef === ref }
}

// CHECK-LABEL: class SWIFT_SYMBOL("s:5Moves5ValueV") Value final {
// CHECK: ~Value() noexcept {
// CHECK-NEXT: if (_isMovedFrom) return;
// CHECK: Value(Value &&other) noexcept {
// CHECK: if (other._isMovedFrom) {
// CHECK: vwTable->initializeWithTake(_getOpaquePointer(), const_cast<char *>(other._getOpaquePointer()), metadata._0);
// CHECK-NEXT: other._isMovedFrom = true;
// CHECK-NEXT: }
// CHECK: Value &operator =(Value &&other) noexcept {
// CHECK-NEXT: if (this == &other) return *this;
// CHECK: if (!_isMovedFrom)
// CHECK-NEXT: vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK: vwTable->initializeWithTake(_getOpaquePointer(), const_cast<char *>(other._getOpaquePointer()), metadata._0);
// CHECK: Value(const Value &other) noexcept {
// CHECK: vwTable->initializeWithCopy
// CHECK: Value &operator =(const Value &other) noexcept {
// CHECK: vwTable->assignWithCopy
// CHECK: bool _isMovedFrom = false;

public enum Choice {
  case value(Value)
  case number(Int)
}

public func makeChoice(_ value: Value) -> Choice { .value(value) }
public func makeArray(_ value: Value) -> [Value] { [value] }
public func makeOptional(_ value: Value) -> Value? { value }
public func borrow(_ value: borrowing Value) -> Int { value.number }
public func borrowGeneric<T>(_ value: borrowing T) -> Int { MemoryLayout<T>.size }
public func identity<T>(_ value: T) -> T { value }
public func update(_ value: inout Value) { value.number += 1 }
