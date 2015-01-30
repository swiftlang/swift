// RUN: rm -rf %t && mkdir -p %t
// RUN: cp -R %S/Inputs/mixed-target %t

// RUN: %target-swift-frontend %clang-importer-sdk -I %S/../Inputs/custom-modules -import-objc-header %t/mixed-target/header.h -emit-module-path %t/MixedWithHeader.swiftmodule %S/Inputs/mixed-with-header.swift %S/../../Inputs/empty.swift -module-name MixedWithHeader
// RUN: %target-swift-frontend %clang-importer-sdk -I %t -I %S/../Inputs/custom-modules -parse %s -verify

// RUN: rm -rf %t/mixed-target/
// RUN: %target-swift-frontend %clang-importer-sdk -I %t -I %S/../Inputs/custom-modules -parse %s -verify

// XFAIL: linux

import MixedWithHeader

func testReexportedClangModules(foo : FooProto) {
  let _: CInt = foo.bar
  let _: CInt = ExternIntX.x
}

func testCrossReferences(derived: Derived) {
  let obj: Base = derived
  let _: NSObject = obj.safeOverride(ForwardClass())
  let _: NSObject = obj.safeOverrideProto(ForwardProtoAdopter())

  testProtocolWrapper(ProtoConformer())
  testStruct(Point(x: 2,y: 3))
}
