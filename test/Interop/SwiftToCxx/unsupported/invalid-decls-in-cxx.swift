// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Invalid -typecheck -verify -experimental-allow-module-with-compiler-errors -clang-header-expose-decls=all-public -emit-clang-header-path %t/invalid.h
// RUN: %FileCheck %s < %t/invalid.h
// RUN: %check-interop-cxx-header-in-clang(%t/invalid.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// A module with errors still gets a header with
// -experimental-allow-module-with-compiler-errors. Invalid declarations are left
// out of it, even when the declaration containing them is valid.

public struct HasInvalidSubscript {
  public var x: Int
  public subscript -> () {} // expected-error {{expected '(' for subscript parameters}}
}

// CHECK-LABEL: class SWIFT_SYMBOL("{{.*}}") HasInvalidSubscript final {
// CHECK-NOT: operator []
// CHECK: SWIFT_INLINE_THUNK swift::Int getX() const
// CHECK-NOT: operator []
// CHECK: {{^}}};

public struct HasInvalidThrowingAccessor<T> {
  public var x: T
  public subscript() -> T {
    get throws(Undefined) { x } // expected-error {{cannot find type 'Undefined' in scope}}
  }
}

// CHECK-LABEL: class SWIFT_SYMBOL("{{.*}}") HasInvalidThrowingAccessor final {
// CHECK-NOT: operator []
// CHECK: SWIFT_INLINE_THUNK T_0_0 getX() const
// CHECK-NOT: operator []
// CHECK: {{^}}};
