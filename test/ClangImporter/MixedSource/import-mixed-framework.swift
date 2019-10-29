// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/mixed-framework/Mixed.framework %t

// Don't crash if a generated header is present but the swiftmodule is missing.
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %t -typecheck %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t/Mixed.framework/Modules/Mixed.swiftmodule/%target-swiftmodule-name %S/Inputs/mixed-framework/Mixed.swift -import-underlying-module -F %t -module-name Mixed -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -F %t -typecheck %s -verify

import Mixed

let instance = SwiftClass(x: 42)
_ = instance.pureSwiftMethod(nil)

let clangStruct = PureClangType(x: 1, y: 2)
instance.categoryMethod(clangStruct)

let x: BogusClass? = nil // expected-error {{'BogusClass' is unavailable: cannot find Swift declaration for this class}}

_ = PureSwiftClass.verify()
_ = Mixed.PureSwiftClass.verify()

let _: CustomName = convertToProto(CustomNameClass())

_ = SwiftClassWithCustomName() // expected-error {{'SwiftClassWithCustomName' has been renamed to 'CustomNameClass'}}

func testAnyObject(_ obj: AnyObject) {
  obj.method()
  _ = obj.integerProperty
  obj.extensionMethod()
  obj.categoryMethod(clangStruct)
  obj.protoMethod()
  _ = obj.protoProperty
}

consumeImplicitlyObjCClass(ImplicitlyObjCClass())

consumeExplicitlyObjCClass(ExplicitlyObjCClass())

consumeHasSameCustomNameClass(HasSameCustomNameClass())

consumeNativeTypeHasDifferentCustomNameClass(SwiftNativeTypeHasDifferentCustomNameClass())
consumeObjCNativeTypeHasDifferentCustomNameClass(NativeTypeHasDifferentCustomNameClass())
consumeNativeTypeHasDifferentCustomNameClass(NativeTypeHasDifferentCustomNameClass()) // expected-error {{cannot convert value of type 'NativeTypeHasDifferentCustomNameClass' to expected argument type 'SwiftNativeTypeHasDifferentCustomNameClass'}}
consumeObjCNativeTypeHasDifferentCustomNameClass(SwiftNativeTypeHasDifferentCustomNameClass()) // expected-error {{cannot convert value of type 'SwiftNativeTypeHasDifferentCustomNameClass' to expected argument type 'NativeTypeHasDifferentCustomNameClass'}}

consumeNativeTypeIsNonObjCClass(SwiftNativeTypeIsNonObjCClass())
consumeNativeTypeIsNonObjCClass(NativeTypeIsNonObjCClass()) // expected-error {{cannot convert value of type 'NativeTypeIsNonObjCClass' to expected argument type 'SwiftNativeTypeIsNonObjCClass'}}

consumeForwardImplicitlyObjCClass(ForwardImplicitlyObjCClass())

consumeForwardExplicitlyObjCClass(ForwardExplicitlyObjCClass())

consumeForwardHasSameCustomNameClass(ForwardHasSameCustomNameClass())

consumeForwardNativeTypeHasDifferentCustomNameClass(SwiftForwardNativeTypeHasDifferentCustomNameClass())
consumeObjCForwardNativeTypeHasDifferentCustomNameClass(ForwardNativeTypeHasDifferentCustomNameClass())
consumeForwardNativeTypeHasDifferentCustomNameClass(ForwardNativeTypeHasDifferentCustomNameClass()) // expected-error {{cannot convert value of type 'ForwardNativeTypeHasDifferentCustomNameClass' to expected argument type 'SwiftForwardNativeTypeHasDifferentCustomNameClass'}}
consumeObjCForwardNativeTypeHasDifferentCustomNameClass(SwiftForwardNativeTypeHasDifferentCustomNameClass()) // expected-error {{cannot convert value of type 'SwiftForwardNativeTypeHasDifferentCustomNameClass' to expected argument type 'ForwardNativeTypeHasDifferentCustomNameClass'}}

consumeForwardNativeTypeIsNonObjCClass(SwiftForwardNativeTypeIsNonObjCClass())
consumeForwardNativeTypeIsNonObjCClass(ForwardNativeTypeIsNonObjCClass()) // expected-error {{cannot convert value of type 'ForwardNativeTypeIsNonObjCClass' to expected argument type 'SwiftForwardNativeTypeIsNonObjCClass'}}

consumeForwardNativeTypeIsUnambiguouslyNonObjCClass(ForwardNativeTypeIsUnambiguouslyNonObjCClass())
