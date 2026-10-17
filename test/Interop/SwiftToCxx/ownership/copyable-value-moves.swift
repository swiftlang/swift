// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Moves -enable-library-evolution -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %FileCheck %s < %t/moves.h
// RUN: %check-interop-cxx-header-in-clang(%t/moves.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// With library evolution, the Swift values below use opaque storage. Small
// values move with their value witness; larger values transfer their box.
// A moved-from object holds no Swift value in either representation.
// CHECK-LABEL: class SWIFT_SYMBOL("s:Sq") Optional final {
// CHECK: Optional(Optional &&other) noexcept {
// CHECK-NEXT: _storage.moveFrom(other._storage, [](char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK: vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: });
// CHECK-NEXT: }

public enum Choice {
  case value(Value)
  case number(Int)
}

// CHECK-LABEL: class SWIFT_SYMBOL("s:5Moves6ChoiceO") Choice final {
// CHECK: Choice(Choice &&other) noexcept {
// CHECK-NEXT: _storage.moveFrom(other._storage, [](char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK: vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: });
// CHECK-NEXT: }

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
// CHECK-NEXT: if (!_storage.isAllocated()) return;
// CHECK: Value(const Value &other) noexcept {
// CHECK-NEXT: if (!other._storage.isAllocated()) return;
// CHECK: vwTable->initializeWithCopy
// CHECK: Value &operator =(const Value &other) noexcept {
// CHECK-NEXT: if (!_storage.isAllocated() || !other._storage.isAllocated()) {
// CHECK-NEXT: *this = Value(other);
// CHECK-NEXT: return *this;
// CHECK-NEXT: }
// CHECK: vwTable->assignWithCopy
// CHECK: Value(Value &&other) noexcept {
// CHECK-NEXT: _storage.moveFrom(other._storage, [](char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK: vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: });
// CHECK-NEXT: }
// CHECK-NEXT: Value &operator =(Value &&other) noexcept {
// CHECK-NEXT: if (this == &other) return *this;
// CHECK-NEXT: if (_storage.isAllocated()) {
// CHECK: vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }
// CHECK-NEXT: _storage.moveFrom(other._storage, [](char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK: vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: });
// CHECK-NEXT: return *this;
// CHECK-NEXT: }
// CHECK: swift::_impl::OpaqueStorage _storage;
// CHECK-NEXT: friend class _impl::_impl_Value;

// Four words force heap storage while Value itself fits in the inline buffer.
public struct LargeValue {
  public var value: Value
  public var padding: Int

  public init(_ ref: Ref, _ number: Int) {
    self.value = Value(ref, number)
    self.padding = 0
  }
}

public func makeChoice(_ value: Value) -> Choice { .value(value) }
public func makeOptional(_ value: Value) -> Value? { value }
public func borrow(_ value: borrowing Value) -> Int { value.number }
public func borrowGeneric<T>(_ value: borrowing T) -> Int { MemoryLayout<T>.size }
public func identity<T>(_ value: T) -> T { value }
public func update(_ value: inout Value) { value.number += 1 }
