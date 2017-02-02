// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: cp -r %S/Inputs/mixed-framework/Mixed.framework %t

// Don't crash if a generated header is present but the swiftmodule is missing.
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %t -typecheck %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t/Mixed.framework/Modules/Mixed.swiftmodule/%target-swiftmodule-name %S/Inputs/mixed-framework/Mixed.swift -import-underlying-module -F %t -module-name Mixed -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %t -typecheck %s -verify -verify-ignore-unknown

// XFAIL: linux

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

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: 'SwiftClassWithCustomName' was obsoleted in Swift 3
