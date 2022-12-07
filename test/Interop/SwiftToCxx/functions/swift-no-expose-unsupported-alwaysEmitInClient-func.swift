// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-decls=all-public -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK-NOT: SWIFT_EXTERN bool $s9Functions24alwaysEmitIntoClientFuncyS2bF(bool x) SWIFT_NOEXCEPT SWIFT_CALL; // alwaysEmitIntoClientFunc(_:)

// CHECK:       namespace Functions __attribute__((swift_private)) {
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT:  } // namespace Functions

// CHECK-NOT: inline bool alwaysEmitIntoClientFunc(bool x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NOT:   return _impl::$s9Functions24alwaysEmitIntoClientFuncyS2bF(x);
// CHECK-NOT: }

@_alwaysEmitIntoClient
public func alwaysEmitIntoClientFunc(_ x: Bool) -> Bool { return !x }
