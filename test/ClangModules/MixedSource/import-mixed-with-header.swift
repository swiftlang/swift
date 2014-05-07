// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift_driver -Xfrontend %clang-importer-sdk -module-cache-path %t -I %S/../Inputs/custom-modules -import-objc-header %S/Inputs/mixed-target/header.h -emit-module-path %t/MixedWithHeader.swiftmodule %S/Inputs/mixed-with-header.swift %S/../../Inputs/empty.swift -module-name MixedWithHeader
// RUN: %swift %clang-importer-sdk -module-cache-path %t -I %S/../Inputs/custom-modules -I %t -parse %s -verify

import MixedWithHeader

func testReexportedClangModules(foo : FooProto) {
  let _: CInt = foo.bar
  let _: CInt = ExternIntX.x
}
