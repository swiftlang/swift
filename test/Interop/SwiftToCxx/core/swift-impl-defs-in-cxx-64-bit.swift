// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core.h
// RUN: %FileCheck %s < %t/core.h

// RUN: %check-interop-cxx-header-in-clang(%t/core.h)

// REQUIRES: PTRSIZE=64

// CHECK: namespace swift SWIFT_PRIVATE_ATTR {

// CHECK: namespace _impl {

// CHECK: // Swift type metadata response type.
// CHECK-NEXT: struct MetadataResponseTy {
// CHECK-NEXT:   void * _Null_unspecified _0;
// CHECK-NEXT:   uint64_t _1;
// CHECK-NEXT: };
// CHECK-NEXT: // Swift type metadata request type.
// CHECK-NEXT: typedef uint64_t MetadataRequestTy;
