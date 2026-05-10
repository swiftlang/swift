// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/ObjcSynthesis/ObjcSynthesis.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t/ObjcSynthesis.framework/Modules/ObjcSynthesis.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name ObjcSynthesis -disable-objc-attr-requires-foundation-module %s
// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name ObjcSynthesis -F %t -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/ObjcSynthesis.symbols.json

// REQUIRES: objc_interop

// CHECK-NOT: "precise": "c:objc(pl)NSCoding(im)initWithCoder"
