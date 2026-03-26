// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core.h
// RUN: %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-os < %t/core.h

// CHECK: SWIFT_INLINE_THUNK void anyAppleOS26Func() noexcept SWIFT_SYMBOL("s:4Core16anyAppleOS26FuncyyF")
// CHECK-macosx-SAME: SWIFT_AVAILABILITY(macos,introduced=26)
// CHECK-linux-gnu-NOT: SWIFT_AVAILABILITY
@available(anyAppleOS 26, *)
public func anyAppleOS26Func() { }

// CHECK: SWIFT_INLINE_THUNK void macOS26Func() noexcept SWIFT_SYMBOL("s:4Core11macOS26FuncyyF")
// CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=26)
@available(macOS 26, *)
public func macOS26Func() { }
