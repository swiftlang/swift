// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/ForwardDeclarations/ForwardDeclarations.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module-path %t/ForwardDeclarations.framework/Modules/ForwardDeclarations.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name ForwardDeclarations -disable-objc-attr-requires-foundation-module %s
// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name ForwardDeclarations -F %t -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/ForwardDeclarations.symbols.json

// REQUIRES: objc_interop

// CHECK: "preciseIdentifier": "c:objc(cs)ForwardDeclaration"
