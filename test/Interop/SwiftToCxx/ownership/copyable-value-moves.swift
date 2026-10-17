// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftmodule -module-name Swift -parse-stdlib -typecheck -cxx-interoperability-mode=default -clang-header-expose-decls=has-expose-attr-or-stdlib -emit-clang-header-path %t/stdlib.h
// RUN: %FileCheck %s --check-prefix=STDLIB < %t/stdlib.h
// RUN: %target-swift-frontend %s -module-name Moves -enable-library-evolution -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %FileCheck %s < %t/moves.h
// RUN: %check-interop-cxx-header-in-clang(%t/moves.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// With library evolution, the Swift values below are boxed. A move transfers
// the box, and a moved-from object has no box. Optional is boxed because its
// layout depends on its generic argument.
// STDLIB-LABEL: class SWIFT_SYMBOL("s:Sq") Optional final {
// STDLIB: Optional(Optional &&other) noexcept : _storage(static_cast<swift::_impl::OpaqueStorage &&>(other._storage)) {}

public enum Choice {
  case value(Value)
  case number(Int)
}

// CHECK-LABEL: class SWIFT_SYMBOL("s:5Moves6ChoiceO") Choice final {
// CHECK: Choice(Choice &&other) noexcept : _storage(static_cast<swift::_impl::OpaqueStorage &&>(other._storage)) {}

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
// CHECK: Value(Value &&other) noexcept : _storage(static_cast<swift::_impl::OpaqueStorage &&>(other._storage)) {}
// CHECK-NEXT: Value &operator =(Value &&other) noexcept {
// CHECK-NEXT: if (this == &other) return *this;
// CHECK-NEXT: if (_storage.isAllocated()) {
// CHECK: vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK-NEXT: }
// CHECK-NEXT: _storage = static_cast<swift::_impl::OpaqueStorage &&>(other._storage);
// CHECK-NEXT: return *this;
// CHECK-NEXT: }
// CHECK: swift::_impl::OpaqueStorage _storage;
// CHECK-NEXT: friend class _impl::_impl_Value;

public func makeChoice(_ value: Value) -> Choice { .value(value) }
public func makeOptional(_ value: Value) -> Value? { value }
public func borrow(_ value: borrowing Value) -> Int { value.number }
public func borrowGeneric<T>(_ value: borrowing T) -> Int { MemoryLayout<T>.size }
public func identity<T>(_ value: T) -> T { value }
public func update(_ value: inout Value) { value.number += 1 }
