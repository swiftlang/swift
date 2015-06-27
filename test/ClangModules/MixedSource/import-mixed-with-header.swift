// RUN: rm -rf %t && mkdir -p %t
// RUN: cp -R %S/Inputs/mixed-target %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/../Inputs/custom-modules -import-objc-header %t/mixed-target/header.h -emit-module-path %t/MixedWithHeader.swiftmodule %S/Inputs/mixed-with-header.swift %S/../../Inputs/empty.swift -module-name MixedWithHeader -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/../Inputs/custom-modules -parse %s -verify

// RUN: rm -rf %t/mixed-target/
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/../Inputs/custom-modules -parse %s -verify

// XFAIL: linux

import MixedWithHeader

func testReexportedClangModules(foo : FooProto) {
  _ = foo.bar as CInt
  _ = ExternIntX.x as CInt
}

func testCrossReferences(derived: Derived) {
  let obj: Base = derived
  _ = obj.safeOverride(ForwardClass()) as NSObject
  _ = obj.safeOverrideProto(ForwardProtoAdopter()) as NSObject

  testProtocolWrapper(ProtoConformer())
  testStruct(Point(x: 2,y: 3))
}
