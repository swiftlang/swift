// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftmodule -module-name Swift -parse-stdlib -typecheck -cxx-interoperability-mode=default -clang-header-expose-decls=has-expose-attr-or-stdlib -emit-clang-header-path %t/core.h
// RUN: %FileCheck %s < %t/core.h

// RUN: %check-interop-cxx-header-in-clang(%t/core.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-private-field -Wno-unused-function)

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
