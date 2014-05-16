// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift_driver -Xfrontend %clang-importer-sdk -module-cache-path %t -I %S/../Inputs/custom-modules -import-objc-header %S/Inputs/mixed-target/header.h -emit-module-path %t/MixedWithHeader.swiftmodule %S/Inputs/mixed-with-header.swift %S/../../Inputs/empty.swift -module-name MixedWithHeader
// RUN: %swift_driver -Xfrontend %clang-importer-sdk -module-cache-path %t -I %t -I %S/../Inputs/custom-modules -import-objc-header %S/Inputs/mixed-target/header-again.h -emit-module-path %t/MixedWithHeaderAgain.swiftmodule %S/Inputs/mixed-with-header-again.swift %S/../../Inputs/empty.swift -module-name MixedWithHeaderAgain
// RUN: %swift %clang-importer-sdk -module-cache-path %t -I %S/../Inputs/custom-modules -I %t -parse %s -verify

import MixedWithHeaderAgain

func testLine(line: Line) {
  testLineImpl(line)
}

func useOriginal(a: ForwardClass, b: Derived, c: ForwardClassUser) {
  let conformer = (c as ProtoConformer)!
  testOriginal(a, b, conformer)
  doSomething(a)
}
