// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -parse %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -emit-ir %s -D IRGEN | FileCheck %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -print-module -source-filename="%s" -module-to-print SwiftPrivateAttr > %t.txt
// RUN: FileCheck -check-prefix=GENERATED-NEGATIVE %s < %t.txt
// RUN: diff -U3 %S/Inputs/SwiftPrivateAttr.txt %t.txt

// Look for identifiers with "Priv" in them that haven't been prefixed.
// GENERATED-NEGATIVE-NOT: {{[^A-Za-z0-9_][A-Za-z0-9]*[Pp]riv}}

import SwiftPrivateAttr

// Note: The long-term plan is for these to only be available from the Swift 
// half of a module, or from an overlay. At that point we should test that these
// are available in that case and /not/ in the normal import case.

// CHECK-LABEL: define void @{{.+}}12testProperty
public func testProperty(foo: Foo) {
  // CHECK: @"\01L_selector(setPrivValue:)"
  _ = foo.__privValue
  foo.__privValue = foo
  
#if !IRGEN
  _ = foo.privValue // expected-error {{'Foo' does not have a member named 'privValue'}}
#endif
}

// CHECK-LABEL: define void @{{.+}}11testMethods
public func testMethods(foo: Foo) {
  // CHECK: @"\01L_selector(noArgs)"
  foo.__noArgs()
  // CHECK: @"\01L_selector(oneArg:)"
  foo.__oneArg(1)
  // CHECK: @"\01L_selector(twoArgs:other:)"
  foo.__twoArgs(1, other: 2)
}

// CHECK-LABEL: define void @{{.+}}16testInitializers
public func testInitializers() {
  // Checked below; look for "CSo3Bar".
  _ = Bar(__noArgs: ())
  _ = Bar(__oneArg: 1)
  _ = Bar(__twoArgs: 1, other: 2)
  _ = Bar(__: 1)
}

// CHECK-LABEL: define void @{{.+}}18testFactoryMethods
public func testFactoryMethods() {
  // CHECK: @"\01L_selector(fooWithOneArg:)"
  _ = Foo(__oneArg: 1)
  // CHECK: @"\01L_selector(fooWithTwoArgs:other:)"
  _ = Foo(__twoArgs: 1, other: 2)
  // CHECK: @"\01L_selector(foo:)"
  _ = Foo(__: 1)
}

#if !IRGEN
public func testSubscript(foo: Foo) {
  _ = foo[foo] // expected-error {{'Foo' does not have a member named 'subscript'}}
  _ = foo[1] // expected-error {{'Foo' does not have a member named 'subscript'}}
}
#endif

// CHECK-LABEL: define void @{{.+}}12testTopLevel
public func testTopLevel() {
  // Checked below; look for "PrivFooSub".
  let foo = __PrivFooSub()
  _ = foo as __PrivProto

  // CHECK: @"\01l_OBJC_PROTOCOL_REFERENCE_$_PrivProto"
  foo.conformsToProtocol(__PrivProto.self)

  // CHECK: call void @privTest()
  __privTest()

  _ = __PrivS1()

#if !IRGEN
  let _ = PrivFooSub() // expected-error {{use of unresolved identifier}}
  privTest() // expected-error {{use of unresolved identifier}}
  PrivS1() // expected-error {{use of unresolved identifier}}
#endif
}

// CHECK-LABEL: define linkonce_odr hidden %swift.type* @_TMaCSo12__PrivFooSub{{.*}} {
// CHECK: %objc_class* @"OBJC_CLASS_$_PrivFooSub"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden {{.+}} @_TTOFCSo3BarcfMS_FT2__VSs5Int32_GSQS__
// CHECK: @"\01L_selector(init:)"
// CHECK-LABEL: define linkonce_odr hidden {{.+}} @_TTOFCSo3BarcfMS_FT9__twoArgsVSs5Int325otherS0__GSQS__
// CHECK: @"\01L_selector(initWithTwoArgs:other:)"
// CHECK-LABEL: define linkonce_odr hidden {{.+}} @_TTOFCSo3BarcfMS_FT8__oneArgVSs5Int32_GSQS__
// CHECK: @"\01L_selector(initWithOneArg:)"
// CHECK-LABEL: define linkonce_odr hidden {{.+}} @_TTOFCSo3BarcfMS_FT8__noArgsT__GSQS__
// CHECK: @"\01L_selector(initWithNoArgs)"

_ = __PrivAnonymousA
_ = __E0PrivA
_ = __PrivE1A as __PrivE1
_ = NSEnum.__PrivA
_ = NSEnum.B
_ = NSOptions.__PrivA
_ = NSOptions.B

func makeSureAnyObject(_: AnyObject) {}
func testCF(a: __PrivCFTypeRef, b: __PrivCFSubRef, c: __PrivInt) {
  makeSureAnyObject(a)
  makeSureAnyObject(b)
#if !IRGEN
  makeSureAnyObject(c) // expected-error {{cannot invoke 'makeSureAnyObject' with an argument list of type '(__PrivInt)'}} expected-note {{expected an argument list of type '(AnyObject)'}}
#endif
}

extension __PrivCFType {}
extension __PrivCFSub {}
_ = 1 as __PrivInt