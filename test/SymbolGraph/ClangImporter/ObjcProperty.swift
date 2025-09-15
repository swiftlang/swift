// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t1)
// RUN: cp -r %S/Inputs/ObjcProperty/ObjcProperty.framework %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t/ObjcProperty.framework/Modules/ObjcProperty.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t -module-name ObjcProperty -disable-objc-attr-requires-foundation-module %s -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name ObjcProperty -F %t -output-dir %t1 -pretty-print -v
// RUN: %validate-json %t/ObjcProperty.symbols.json %t/ObjcProperty.formatted.symbols.json
// RUN: %FileCheck %s --input-file %t/ObjcProperty.formatted.symbols.json
// RUN: %FileCheck %s --input-file %t/ObjcProperty.formatted.symbols.json --check-prefix XLANG
// RUN: %FileCheck %s --input-file %t1/ObjcProperty.symbols.json
// RUN: %FileCheck %s --input-file %t1/ObjcProperty.symbols.json --check-prefix XLANG

// REQUIRES: objc_interop

public enum SwiftEnum {}

public class SwiftClass : Foo {}

// ensure we don't accidentaly pull in exported objc modules

// CHECK-NOT: "precise": "c:objc(cs)NSString"

// ensure that synthesized inherited objc symbols do not appear in the symbol graph

// CHECK-NOT:       "c:objc(cs)NSObject(im)init"

// ensure that children of clang nodes appear in the symbol graph

// CHECK: "precise": "c:objc(cs)Foo(py)today"

// ensure that a swift class that inherits from an objc class still doesn't generate inherited
// symbols

// XLANG-COUNT-2: selectDate:
