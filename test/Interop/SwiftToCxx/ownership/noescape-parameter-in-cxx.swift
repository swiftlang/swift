// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name NoEsc -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/noesc.h
// RUN: %FileCheck %s < %t/noesc.h
// RUN: %check-interop-cxx-header-in-clang(%t/noesc.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public struct ValueType { public var x: Int64 = 0 }
public final class RefType { public init() {} }

public func takeBorrowedValue(_ x: borrowing ValueType) {}
public func takeDefaultValue(_ x: ValueType) {}
public func takeInoutValue(_ x: inout ValueType) {}
public func takeConsumingValue(_ x: consuming ValueType) {}

public func takeBorrowedRef(_ x: borrowing RefType) {}
public func takeInoutRef(_ x: inout RefType) {}

public func takeInt(_ x: Int) {}
public func takeInoutInt(_ x: inout Int) {}

public func takeGeneric<T>(_ x: T) {}

// CHECK: SWIFT_EXTERN void $s5NoEsc12takeInoutIntyySizF(ptrdiff_t * _Nonnull SWIFT_NOESCAPE x) SWIFT_NOEXCEPT SWIFT_CALL; // takeInoutInt(_:)

// CHECK: SWIFT_INLINE_THUNK void takeBorrowedRef(const RefType& x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK: SWIFT_INLINE_THUNK void takeBorrowedValue(const ValueType& SWIFT_NOESCAPE x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK: SWIFT_INLINE_THUNK void takeConsumingValue(const ValueType& SWIFT_NOESCAPE x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK: SWIFT_INLINE_THUNK void takeDefaultValue(const ValueType& SWIFT_NOESCAPE x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK: SWIFT_INLINE_THUNK void takeGeneric(const T_0_0& x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK: SWIFT_INLINE_THUNK void takeInoutInt(swift::Int & SWIFT_NOESCAPE x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK: SWIFT_INLINE_THUNK void takeInoutRef(RefType& x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK: SWIFT_INLINE_THUNK void takeInoutValue(ValueType& SWIFT_NOESCAPE x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK: SWIFT_INLINE_THUNK void takeInt(swift::Int x) noexcept SWIFT_SYMBOL({{.*}}) {
