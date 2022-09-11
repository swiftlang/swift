// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h
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

// CHECK: class B;
// CHECK-NEXT: namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_A;

// CHECK: class A final {

// CHECK: B returnsB() const;

// CHECK: class A;
// CHECK-NEXT: namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_B;

// CHECK: class B final {

// CHECK: inline B A::returnsB() const {
// CHECK: inline A B::returnsA() const {
