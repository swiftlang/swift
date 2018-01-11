// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 3 -find-mangled '$SSo11SomeCStructV' | %FileCheck -check-prefix=CHECK-TOP-ALIAS-3 %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled '$SSo11SomeCStructV' | %FileCheck -check-prefix=CHECK-TOP-ALIAS-4 %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 3 -find-mangled '$SSo13InnerInSwift4V' | %FileCheck -check-prefix=CHECK-NESTED-ALIAS-3 %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled '$SSo13InnerInSwift4V' | %FileCheck -check-prefix=CHECK-NESTED-ALIAS-4 %s

import APINotesFrameworkTest

// CHECK-TOP-ALIAS-3: typealias ImportantCStruct = VeryImportantCStruct
// CHECK-TOP-ALIAS-4: struct VeryImportantCStruct {
// CHECK-NESTED-ALIAS-3: typealias InnerInSwift4 = Outer.Inner
// CHECK-NESTED-ALIAS-4: struct Inner {
