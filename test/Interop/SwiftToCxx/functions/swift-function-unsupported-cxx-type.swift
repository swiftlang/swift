// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-decls=all-public -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

public func a() { }
public func b(_ x: @escaping (Int) -> ()) { }
public func c() {}

// CHECK: SWIFT_EXTERN void $s9Functions1ayyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // a()
// CHECK: SWIFT_EXTERN void $s9Functions1cyyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // c()

// CHECK: inline void a() noexcept SWIFT_SYMBOL("s:9Functions1ayyF") {
// CHECK: inline void c() noexcept SWIFT_SYMBOL("s:9Functions1cyyF") {
// CHECK-NOT: b(
