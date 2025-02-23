// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/Inputs/custom-modules -typecheck %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/Inputs/custom-modules -emit-ir %s -D IRGEN | %FileCheck %s

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
  _ = foo[foo] // expected-error {{value of type 'Foo' has no subscripts}}
  _ = foo[1] // expected-error {{value of type 'Foo' has no subscripts}}
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
  let _ = PrivFooSub() // expected-error {{cannot find 'PrivFooSub' in scope}}
  privTest() // expected-error {{cannot find 'privTest' in scope}}
  PrivS1() // expected-error {{cannot find 'PrivS1' in scope}}
#endif
}

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
  var _: __PrivCFTypeRef // expected-error {{'__PrivCFTypeRef' has been renamed to '__PrivCFType'}}
  var _: __PrivCFSubRef // expected-error {{'__PrivCFSubRef' has been renamed to '__PrivCFSub'}}
}
#endif

func testCF(_ a: __PrivCFType, b: __PrivCFSub, c: __PrivInt) {
  makeSureAnyObject(a)
  makeSureAnyObject(b)
#if !IRGEN
  makeSureAnyObject(c) // expected-error {{argument type '__PrivInt' (aka 'Int32') expected to be an instance of a class or class-constrained type}}
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

// CHECK-LABEL: define linkonce_odr hidden {{.+}} @"$sSo3BarC8__noArgsABSgyt_tcfcTO"
// CHECK: @"\01L_selector(initWithNoArgs)"
// CHECK-LABEL: define linkonce_odr hidden {{.+}} @"$sSo3BarC8__oneArgABSgs5Int32V_tcfcTO"
// CHECK: @"\01L_selector(initWithOneArg:)"
// CHECK-LABEL: define linkonce_odr hidden {{.+}} @"$sSo3BarC9__twoArgs5otherABSgs5Int32V_AGtcfcTO"
// CHECK: @"\01L_selector(initWithTwoArgs:other:)"
// CHECK-LABEL: define linkonce_odr hidden {{.+}} @"$sSo3BarC2__ABSgs5Int32V_tcfcTO"
// CHECK: @"\01L_selector(init:)"
