// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-c-header-in-clang(%t/functions.h)

// REQUIRES: OS=macosx || OS=linux-gnu

// CHECK: SWIFT_EXTERN long $s9Functions16passThroughCLongyS2iF(long x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: SWIFT_EXTERN unsigned long $s9Functions24passThroughCUnsignedLongyS2uF(unsigned long x) SWIFT_NOEXCEPT SWIFT_CALL;

public func passThroughCLong(_ x: CLong) -> CLong { return x }

public func passThroughCUnsignedLong(_ x: CUnsignedLong) -> CUnsignedLong { return x }
