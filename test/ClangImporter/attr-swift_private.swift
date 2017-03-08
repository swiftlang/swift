// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/Inputs/custom-modules -typecheck %s -verify -verify-ignore-unknown
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -Xllvm -new-mangling-for-tests -I %S/Inputs/custom-modules -emit-ir %s -D IRGEN | %FileCheck %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/Inputs/custom-modules -print-module -source-filename="%s" -module-to-print SwiftPrivateAttr > %t.txt
// RUN: %FileCheck -check-prefix=GENERATED-NEGATIVE %s < %t.txt
// RUN: diff -U3 %S/Inputs/SwiftPrivateAttr.txt %t.txt

// Look for identifiers with "Priv" in them that haven't been prefixed.
// GENERATED-NEGATIVE-NOT: {{[^A-Za-z0-9_][A-Za-z0-9]*[Pp]riv}}

// REQUIRES: objc_interop

import SwiftPrivateAttr

// Note: The long-term plan is for these to only be available from the Swift 
// half of a module, or from an overlay. At that point we should test that these
// are available in that case and /not/ in the normal import case.

// CHECK-LABEL: define{{( protected)?}} swiftcc void @{{.+}}12testProperty
public func testProperty(_ foo: Foo) {
  // CHECK: @"\01L_selector(setPrivValue:)"
  _ = foo.__privValue
  foo.__privValue = foo

  // CHECK: @"\01L_selector(setPrivClassValue:)"
  _ = Foo.__privClassValue
  Foo.__privClassValue = foo
  
#if !IRGEN
  _ = foo.privValue // expected-error {{value of type 'Foo' has no member 'privValue'}}
#endif
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @{{.+}}11testMethods
public func testMethods(_ foo: Foo) {
  // CHECK: @"\01L_selector(noArgs)"
  foo.__noArgs()
  // CHECK: @"\01L_selector(oneArg:)"
  foo.__oneArg(1)
  // CHECK: @"\01L_selector(twoArgs:other:)"
  foo.__twoArgs(1, other: 2)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @{{.+}}16testInitializers
public func testInitializers() {
  // Checked below; look for "CSo3Bar".
  _ = Bar(__noArgs: ())
  _ = Bar(__oneArg: 1)
  _ = Bar(__twoArgs: 1, other: 2)
  _ = Bar(__: 1)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @{{.+}}18testFactoryMethods
public func testFactoryMethods() {
  // CHECK: @"\01L_selector(fooWithOneArg:)"
  _ = Foo(__oneArg: 1)
  // CHECK: @"\01L_selector(fooWithTwoArgs:other:)"
  _ = Foo(__twoArgs: 1, other: 2)
  // CHECK: @"\01L_selector(foo:)"
  _ = Foo(__: 1)
}

#if !IRGEN
public func testSubscript(_ foo: Foo) {
  _ = foo[foo] // expected-error {{type 'Foo' has no subscript members}}
  _ = foo[1] // expected-error {{type 'Foo' has no subscript members}}
}
#endif

// CHECK-LABEL: define{{( protected)?}} swiftcc void @{{.+}}12testTopLevel
public func testTopLevel() {
  // Checked below; look for "PrivFooSub".
  let foo = __PrivFooSub()
  _ = foo as __PrivProto

  // CHECK: @"\01l_OBJC_PROTOCOL_REFERENCE_$_PrivProto"
  foo.conforms(to: __PrivProto.self)

  // CHECK: call void @privTest()
  __privTest()

  _ = __PrivS1()

#if !IRGEN
  let _ = PrivFooSub() // expected-error {{use of unresolved identifier}}
  privTest() // expected-error {{use of unresolved identifier}}
  PrivS1() // expected-error {{use of unresolved identifier}}
#endif
}

// CHECK-LABEL: define linkonce_odr hidden %swift.type* @_T0So12__PrivFooSubCMa{{.*}} {
// CHECK: %objc_class** @"OBJC_CLASS_REF_$_PrivFooSub"
// CHECK: }

// CHECK-LABEL: define linkonce_odr hidden {{.+}} @_T0So3BarCSQyABGs5Int32V2___tcfcTO
// CHECK: @"\01L_selector(init:)"
// CHECK-LABEL: define linkonce_odr hidden {{.+}} @_T0So3BarCSQyABGs5Int32V9__twoArgs_AE5othertcfcTO
// CHECK: @"\01L_selector(initWithTwoArgs:other:)"
// CHECK-LABEL: define linkonce_odr hidden {{.+}} @_T0So3BarCSQyABGs5Int32V8__oneArg_tcfcTO
// CHECK: @"\01L_selector(initWithOneArg:)"
// CHECK-LABEL: define linkonce_odr hidden {{.+}} @_T0So3BarCSQyABGyt8__noArgs_tcfcTO
// CHECK: @"\01L_selector(initWithNoArgs)"

_ = __PrivAnonymousA
_ = __E0PrivA
_ = __PrivE1A as __PrivE1
_ = NSEnum.__privA
_ = NSEnum.B
_ = NSOptions.__privA
_ = NSOptions.B

func makeSureAnyObject(_: AnyObject) {}

#if !IRGEN
func testUnavailableRefs() {
  var x: __PrivCFTypeRef // expected-error {{'__PrivCFTypeRef' has been renamed to '__PrivCFType'}}
  var y: __PrivCFSubRef // expected-error {{'__PrivCFSubRef' has been renamed to '__PrivCFSub'}}
}
#endif

func testCF(_ a: __PrivCFType, b: __PrivCFSub, c: __PrivInt) {
  makeSureAnyObject(a)
  makeSureAnyObject(b)
#if !IRGEN
  makeSureAnyObject(c) // expected-error {{argument type '__PrivInt' (aka 'Int32') does not conform to expected type 'AnyObject'}}
#endif
}

extension __PrivCFType {}
extension __PrivCFSub {}
_ = 1 as __PrivInt

#if !IRGEN
func testRawNames() {
  let _ = Foo.__fooWithOneArg(0) // expected-error {{'__fooWithOneArg' has been replaced by 'init(__oneArg:)'}}
  let _ = Foo.__foo // expected-error{{'__foo' has been replaced by 'init(__:)'}}
}
#endif

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: '__PrivCFTypeRef' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: '__PrivCFSubRef' was obsoleted in Swift 3
// <unknown>:0: error: unexpected note produced: '__fooWithOneArg' has been explicitly marked unavailable here
// <unknown>:0: error: unexpected note produced: '__foo' has been explicitly marked unavailable here
