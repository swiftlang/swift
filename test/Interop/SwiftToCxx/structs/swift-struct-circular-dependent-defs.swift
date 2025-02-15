// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h)

struct Large {
    let x: (Int64, Int64, Int64, Int64, Int64, Int64) = (0, 0, 0, 0, 0, 0)
}

public struct A {
    public func returnsB() -> B {
        return B()
    }

    let v: Large = Large()
}

public struct B {
    public func returnsA() -> A {
        return A()
    }

    let v: Large = Large()
}

// CHECK: class SWIFT_SYMBOL({{.*}}) B;
// CHECK: class SWIFT_SYMBOL({{.*}}) A;
// CHECK: namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_A;

// CHECK: class SWIFT_SYMBOL({{.*}}) A final {

// CHECK: B returnsB() const SWIFT_SYMBOL({{.*}});

// CHECK: namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_A {

// CHECK: namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_B;

// CHECK: class SWIFT_SYMBOL({{.*}}) B final {

// CHECK: SWIFT_INLINE_THUNK B A::returnsB() const {
// CHECK: SWIFT_INLINE_THUNK A B::returnsA() const {
