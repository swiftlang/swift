// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Int128Module -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/int128.h
// RUN: %FileCheck %s < %t/int128.h

// RUN: %check-interop-cxx-header-in-clang(%t/int128.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: PTRSIZE=64

// CHECK:      SWIFT_INLINE_THUNK __int128 passThroughInt128(__int128 x) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT{{.*}} {
// CHECK-NEXT:   return Int128Module::_impl::{{.*}}(x);
// CHECK-NEXT: }

// CHECK:      SWIFT_INLINE_THUNK unsigned __int128 passThroughUInt128(unsigned __int128 x) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT{{.*}} {
// CHECK-NEXT:   return Int128Module::_impl::{{.*}}(x);
// CHECK-NEXT: }

@available(SwiftStdlib 6.0, *)
public func passThroughInt128(_ x: Int128) -> Int128 { return x }

@available(SwiftStdlib 6.0, *)
public func passThroughUInt128(_ x: UInt128) -> UInt128 { return x }
