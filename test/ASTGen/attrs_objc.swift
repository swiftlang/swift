// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -dump-parse -import-objc-header %S/Inputs/objc_decls.h -enable-experimental-feature ParserASTGen > %t/astgen.ast.raw
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -dump-parse -import-objc-header %S/Inputs/objc_decls.h > %t/cpp-parser.ast.raw

// Filter out any addresses in the dump, since they can differ.
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/cpp-parser.ast.raw > %t/cpp-parser.ast
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/astgen.ast.raw > %t/astgen.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast
// RUN: %target-typecheck-verify-swift %clang-importer-sdk -import-objc-header %S/Inputs/objc_decls.h -enable-experimental-feature ParserASTGen 

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ParserASTGen

@objc class MyCls {
  @objc(theValue) var value: Int { 1 }
  @objc(barWithX:Y:) func foo(x: Int, y: Int) {}
}

@objc @implementation extension ObjCClass {}
@objc @implementation(Category1) extension ObjCClass {} // expected-error {{Objective-C category should be specified on '@objc', not '@implementation'}}
@objc(Category2) @implementation extension ObjCClass {}

// FIXME: @_objcImplementation inserts implicit @objc attribute in C++ parser.
//@_objcImplementation extension ObjCClass2 {} // xpected-error {{cannot find type 'ObjCClass2' in scope}}
//@_objcImplementation(Category) extension ObjCClass2 {} // xpected-error {{cannot find type 'ObjCClass2' in scope}}

@_objcRuntimeName(RenamedClass) class ThisWillBeRenamed {}

@_swift_native_objc_runtime_base(NSMagicBase) class TestNativeObjCRuntimeBase {}
