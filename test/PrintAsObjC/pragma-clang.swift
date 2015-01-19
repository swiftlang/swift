// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %s -import-objc-header %S/Inputs/bridging-header.h %clang-importer-sdk -parse -emit-objc-header-path %t/pragma-clang.h
// RUN: %check-in-clang -fsyntax-only -Werror %t/pragma-clang.h
// RUN: FileCheck %s < %t/pragma-clang.h

// CHECK: #pragma clang diagnostic push
// CHECK: #pragma clang diagnostic ignored "-Wproperty-attribute-mismatch"
// CHECK: @interface Test
@objc class Test : TestProto {
  var str: String = ""
  var strongProp: AnyObject?
}
// CHECK: #pragma clang diagnostic pop
// CHECK-NOT: clang diagnostic
