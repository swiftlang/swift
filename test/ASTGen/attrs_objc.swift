// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s %dump-parse -import-objc-header %S/Inputs/objc_decls.h -enable-experimental-feature ParserASTGen \
// RUN:   | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s %dump-parse -import-objc-header %S/Inputs/objc_decls.h \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

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

@objc @implementation extension ObjCClass {
    var theValue: String? {
        get { "" }
        set {}
    }
    @objc(methodWithX:Y:)
    func methodWith(x: Int, y: Int) {}
}
@objc @implementation(Category1) extension ObjCClass {} // expected-error {{Objective-C category should be specified on '@objc', not '@implementation'}}
@objc(Category2) @implementation extension ObjCClass {}

@_objcImplementation extension ObjCClass2 {} // expected-warning {{'@_objcImplementation' is deprecated; use '@implementation' instead}}

@_objcRuntimeName(RenamedClass) class ThisWillBeRenamed {}

@_swift_native_objc_runtime_base(NSMagicBase) class TestNativeObjCRuntimeBase {}

func testPoundObjC() {
  let _: String = #keyPath(ObjCClass.theValue)
  let _: Selector = #selector(getter:ObjCClass.theValue)
  let _: Selector = #selector(ObjCClass.methodWith(x:y:))
}
