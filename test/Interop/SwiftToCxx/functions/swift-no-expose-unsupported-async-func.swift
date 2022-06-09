// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -disable-availability-checking -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK-NOT: SWIFT_EXTERN double $s9Functions9asyncFuncyS2dYaF(double x) SWIFT_NOEXCEPT SWIFT_CALL; // asyncFunc(_:)

// CHECK:       namespace Functions {
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT:  } // namespace Functions

// CHECK-NOT: inline double asyncFunc(double x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NOT:   return _impl::$s9Functions9asyncFuncyS2dYaF(x);
// CHECK-NOT: }

// REQUIRES: concurrency

public func asyncFunc(_ x: Double) async -> Double { return 2 * x }
