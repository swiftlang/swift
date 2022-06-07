// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK-NOT: SWIFT_EXTERN double $s9Functions10async_funcyS2dYaF(double x) SWIFT_NOEXCEPT SWIFT_CALL; // async_func(_:)
// CHECK-NOT: SWIFT_EXTERN bool $s9Functions16transparent_funcyS2bF(bool x) SWIFT_NOEXCEPT SWIFT_CALL; // transparent_func(_:)

// CHECK:       namespace Functions {
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT:  } // namespace Functions

// CHECK-NOT:  inline double async_func(double x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NOT:   return _impl::$s9Functions10async_funcyS2dYaF(x);
// CHECK-NOT: }

// CHECK-NOT:  inline bool transparent_func(bool x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NOT:   return _impl::$s9Functions16transparent_funcyS2bF(x);
// CHECK-NOT: }

public func async_func(_ x: Double) async -> Double { return 2 * x }

@_transparent
public func transparent_func(_ x: Bool) -> Bool { return !x }
