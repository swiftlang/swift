// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %s -import-objc-header %S/Inputs/bridging-header.h %clang-importer-sdk -module-cache-path %t/clang-module-cache -parse -emit-objc-header-path %t/pragma-clang.h
// RUN: FileCheck %s < %t/pragma-clang.h

// CHECK: #pragma clang diagnostic push
// CHECK-NEXT: #pragma clang diagnostic ignored "-Wproperty-attribute-mismatch"
@objc class Test : TestProto {
  var str: String = ""
  var strongProp: AnyObject?
}
// CHECK: #pragma clang diagnostic pop

