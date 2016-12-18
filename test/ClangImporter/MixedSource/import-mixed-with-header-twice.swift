// RUN: rm -rf %t && mkdir -p %t
// RUN: cp -R %S/Inputs/mixed-target %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/../Inputs/custom-modules -import-objc-header %t/mixed-target/header.h -emit-module-path %t/MixedWithHeader.swiftmodule %S/Inputs/mixed-with-header.swift %S/../../Inputs/empty.swift -module-name MixedWithHeader -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t -I %S/../Inputs/custom-modules -import-objc-header %t/mixed-target/header-again.h -emit-module-path %t/MixedWithHeaderAgain.swiftmodule %S/Inputs/mixed-with-header-again.swift %S/../../Inputs/empty.swift -module-name MixedWithHeaderAgain -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/../Inputs/custom-modules -I %t -typecheck %s -verify

// RUN: rm %t/mixed-target/header.h
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t -I %S/../Inputs/custom-modules -typecheck %s 2>&1 | %FileCheck %s -check-prefix=USE-SERIALIZED-HEADER

// XFAIL: linux

// USE-SERIALIZED-HEADER: redefinition of 'Point2D'
// USE-SERIALIZED-HEADER: previous definition is here

import MixedWithHeaderAgain

func testLine(line: Line) {
  testLineImpl(line)
}

func useOriginal(a: ForwardClass, b: Derived, c: ForwardClassUser) {
  let conformer = c as! ProtoConformer
  testOriginal(a, b, conformer)
  doSomething(a)
}
