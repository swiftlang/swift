// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) %s -import-objc-header %S/Inputs/bridging-header.h -typecheck -verify -emit-objc-header-path %t/pragma-clang.h
// RUN: %check-in-clang -fsyntax-only -Werror %t/pragma-clang.h
// RUN: %FileCheck %s < %t/pragma-clang.h

// REQUIRES: objc_interop

// CHECK: #pragma clang diagnostic push
// CHECK: #pragma clang diagnostic ignored "-Wproperty-attribute-mismatch"
// CHECK: @interface Test
@objc class Test : NSObject, TestProto {
  var str: String = ""
  var strongProp: Any?
}
// CHECK: #pragma clang diagnostic pop
// CHECK-NOT: clang diagnostic
