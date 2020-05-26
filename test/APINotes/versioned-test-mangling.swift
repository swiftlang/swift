// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled '$sSo11SomeCStructV' | %FileCheck -check-prefix=CHECK-TOP-ALIAS-4 %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 5 -find-mangled '$sSo11SomeCStructV' | %FileCheck -check-prefix=CHECK-TOP-ALIAS-5 %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled '$sSo13InnerInSwift5V' | %FileCheck -check-prefix=CHECK-NESTED-ALIAS-4 %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 5 -find-mangled '$sSo13InnerInSwift5V' | %FileCheck -check-prefix=CHECK-NESTED-ALIAS-5 %s

import APINotesFrameworkTest

// CHECK-TOP-ALIAS-4: typealias ImportantCStruct = VeryImportantCStruct
// CHECK-TOP-ALIAS-5: struct VeryImportantCStruct {
// CHECK-NESTED-ALIAS-4: typealias InnerInSwift5 = Outer.Inner
// CHECK-NESTED-ALIAS-5: struct Inner {
