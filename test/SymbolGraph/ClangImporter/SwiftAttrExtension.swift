// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/SwiftAttrExtension/SwiftAttrExtension.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module-path %t/SwiftAttrExtension.framework/Modules/SwiftAttrExtension.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name SwiftAttrExtension -disable-objc-attr-requires-foundation-module %s -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %FileCheck %s --input-file %t/SwiftAttrExtension.symbols.json

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name SwiftAttrExtension -F %t -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/SwiftAttrExtension.symbols.json

// REQUIRES: objc_interop

extension MyObjcClass {
  public func someFunc() {}
}

// CHECK: s:So11MyObjcClassC18SwiftAttrExtensionE8someFuncyyF
