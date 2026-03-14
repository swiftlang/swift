
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -batch-code-completion -source-filename %t/test.swift -filecheck %raw-FileCheck -completion-output-dir %t/out -code-completion-annotate-results -import-objc-header %t/ObjC.h -enable-objc-interop %t/Lib.swift 
// REQUIRES: objc_interop


//--- ObjC.h
@import Foundation;

typedef NS_OPTIONS(NSInteger, MyOptions) {
  MyOptOne = 1 << 0,
  MyOptTwo = 1 << 1,
};

@interface MyObj : NSObject
// 'opt' should not be defaulted.
// FIXME: Currently this is considered defaulted because the base name is 'store'.
- (void)storeOptions:(MyOptions)opts;

// 'opts' should not be defaulted.
- (void)addOptions:(NSDictionary*)opts;

// 'attrs' should not be defaulted.
- (void)addAttributes:(NSDictionary *)attrs;

// 'info' should not be defaulted but 'opts' should be.
- (void)addUserInfo:(NSDictionary *)info options:(MyOptions)opts;

// 'opts' should be defaulted because the base name is 'run'.
- (void)runWithOptions:(MyOptions)opts;

// 'attrs' should be defaulted because the base name is 'execute'. 
- (void)executeWithAttributes:(NSDictionary *)attrs;
@end

//--- Lib.swift
extension MyObj {
  // 'attrs' should not be defaulted because this is explicitly written in Swift.
  func swift_addAttributes(_ attrs : [AnyHashable:Any]! = [:]) {}
}

//--- test.swift
func test(value: MyObj) {
  value.#^COMPLETE^#
// COMPLETE-NOT: name=addOptions()
// COMPLETE-NOT: name=addAttributes()

// FIXME: we don't want to suggest 'store()'.
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>store</name>(); typename=<typeid.sys>Void</typeid.sys>; name=store()
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>store</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>opts</callarg.param>: <callarg.type><typeid.user>MyOptions</typeid.user></callarg.type><callarg.default/></callarg>); typename=<typeid.sys>Void</typeid.sys>; name=store(:)
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>addOptions</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>opts</callarg.param>: <callarg.type>[<typeid.sys>AnyHashable</typeid.sys> : <keyword>Any</keyword>]!</callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>; name=addOptions(:)
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>addAttributes</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>attrs</callarg.param>: <callarg.type>[<typeid.sys>AnyHashable</typeid.sys> : <keyword>Any</keyword>]!</callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>; name=addAttributes(:)
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>addUserInfo</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>info</callarg.param>: <callarg.type>[<typeid.sys>AnyHashable</typeid.sys> : <keyword>Any</keyword>]!</callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>; name=addUserInfo(:)
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>addUserInfo</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>info</callarg.param>: <callarg.type>[<typeid.sys>AnyHashable</typeid.sys> : <keyword>Any</keyword>]!</callarg.type></callarg>, <callarg><callarg.label>options</callarg.label> <callarg.param>opts</callarg.param>: <callarg.type><typeid.user>MyOptions</typeid.user></callarg.type><callarg.default/></callarg>); typename=<typeid.sys>Void</typeid.sys>; name=addUserInfo(:options:)
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>run</name>(); typename=<typeid.sys>Void</typeid.sys>; name=run()
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>run</name>(<callarg><callarg.label>options</callarg.label> <callarg.param>opts</callarg.param>: <callarg.type><typeid.user>MyOptions</typeid.user></callarg.type><callarg.default/></callarg>); typename=<typeid.sys>Void</typeid.sys>; name=run(options:)
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>execute</name>(); typename=<typeid.sys>Void</typeid.sys>; name=execute()
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>execute</name>(<callarg><callarg.label>attributes</callarg.label> <callarg.param>attrs</callarg.param>: <callarg.type>[<typeid.sys>AnyHashable</typeid.sys> : <keyword>Any</keyword>]!</callarg.type><callarg.default/></callarg>); typename=<typeid.sys>Void</typeid.sys>; name=execute(attributes:)
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>swift_addAttributes</name>(); typename=<typeid.sys>Void</typeid.sys>; name=swift_addAttributes()
// COMPLETE-DAG: Decl[InstanceMethod]/CurrNominal:   <name>swift_addAttributes</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>attrs</callarg.param>: <callarg.type>[<typeid.sys>AnyHashable</typeid.sys> : <keyword>Any</keyword>]!</callarg.type><callarg.default/></callarg>); typename=<typeid.sys>Void</typeid.sys>; name=swift_addAttributes(:)

// COMPLETE-NOT: name=addOptions()
// COMPLETE-NOT: name=addAttributes()
}

