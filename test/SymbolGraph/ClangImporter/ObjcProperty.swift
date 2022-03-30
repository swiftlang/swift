// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/ObjcProperty/ObjcProperty.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t/ObjcProperty.framework/Modules/ObjcProperty.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name ObjcProperty -disable-objc-attr-requires-foundation-module %s
// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name ObjcProperty -F %t -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/ObjcProperty.symbols.json
// RUN: %FileCheck %s --input-file %t/ObjcProperty.symbols.json --check-prefix XLANG

// REQUIRES: objc_interop

import Foundation

public enum SwiftEnum {}

public class SwiftClass : Foo {}

// ensure that synthesized inherited objc symbols do not appear in the symbol graph

// CHECK-NOT:       "c:objc(cs)NSObject(im)init"

// ensure that children of clang nodes appear in the symbol graph

// CHECK: "precise": "c:objc(cs)Foo(py)today"

// ensure that a swift class that inherits from an objc class still doesn't generate inherited
// symbols

// XLANG-COUNT-2: selectDate:
