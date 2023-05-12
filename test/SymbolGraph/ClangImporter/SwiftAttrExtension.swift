// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/SwiftAttrExtension)
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -emit-module-path %t/SwiftAttrExtension.swiftmodule -enable-objc-interop -I %t/SwiftAttrExtension -module-name SwiftAttrExtension -import-underlying-module -emit-symbol-graph -emit-symbol-graph-dir %t %t/Extension.swift
// RUN: %FileCheck %s --input-file %t/SwiftAttrExtension.symbols.json

//--- SwiftAttrExtension/module.modulemap
module SwiftAttrExtension {
  header "SwiftAttrExtension.h"
}

//--- SwiftAttrExtension/SwiftAttrExtension.h
@import Foundation;

__attribute__((swift_attr("@_nonSendable(_assumed)")))
@interface MyObjcClass : NSObject
@end

//--- Extension.swift
extension MyObjcClass {
  // CHECK: s:So11MyObjcClassC18SwiftAttrExtensionE8someFuncyyF
  public func someFunc() {}
}
