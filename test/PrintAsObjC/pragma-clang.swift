// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -import-objc-header %S/Inputs/bridging-header.h -parse -emit-objc-header-path %t/pragma-clang.h
// RUN: %check-in-clang -fsyntax-only -Werror %t/pragma-clang.h
// RUN: FileCheck %s < %t/pragma-clang.h

// REQUIRES: objc_interop

// CHECK: #pragma clang diagnostic push
// CHECK: #pragma clang diagnostic ignored "-Wproperty-attribute-mismatch"
// CHECK: @interface Test
@objc class Test : NSObject, TestProto {
  var str: String = ""
  var strongProp: AnyObject?
}
// CHECK: #pragma clang diagnostic pop
// CHECK-NOT: clang diagnostic
