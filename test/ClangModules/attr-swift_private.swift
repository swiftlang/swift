// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -parse %s -verify
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -emit-ir %s | FileCheck %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -print-module -source-filename="%s" -module-to-print SwiftPrivateAttr > %t.txt
// RUN: FileCheck -check-prefix=GENERATED-NEGATIVE %s < %t.txt
// RUN: diff -U3 %S/Inputs/SwiftPrivateAttr.txt %t.txt

// Look for identifiers with "Priv" in them that haven't been prefixed.
// GENERATED-NEGATIVE-NOT: {{[^A-Za-z0-9_][A-Za-z0-9]*[Pp]riv}}

import SwiftPrivateAttr

// Note: The long-term plan is for these to only be available from the Swift 
// half of a module, or from an overlay. At that point we should test that these
// are available in that case and /not/ in the normal import case.

public func test(foo: Foo) {
  // CHECK: @"\01L_selector(setPrivValue:)"
  _ = foo.__privValue
  foo.__privValue = foo
}

// __PrivFooSub() as __PrivProto
// __privTest()
// __PrivS1()
//
// _ = __PrivAnonymousA
// _ = __E0PrivA
// _ = __PrivE1A as __PrivE1
_ = NSEnum.__PrivA
_ = NSEnum.B
_ = NSOptions.__PrivA
_ = NSOptions.B

// extension __PrivCFType {}
// extension __PrivCFSub {}
// _ = 1 as __PrivInt