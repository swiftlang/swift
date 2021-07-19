// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/ObjcProperty/ObjcProperty.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t/ObjcProperty.framework/Modules/ObjcProperty.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name ObjcProperty -disable-objc-attr-requires-foundation-module %s
// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name ObjcProperty -F %t -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/ObjcProperty.symbols.json

// REQUIRES: objc_interop

import Foundation

public enum SwiftEnum {}

// CHECK: "precise": "c:objc(cs)Foo(py)today"
