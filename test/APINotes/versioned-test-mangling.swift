// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled '$sSo11SomeCStructV' | %FileCheck -check-prefix=CHECK-TOP-ALIAS-UNDERLYING-4 %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 5 -find-mangled '$sSo11SomeCStructV' | %FileCheck -check-prefix=CHECK-TOP-ALIAS-UNDERLYING-5 %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled '$sSo13InnerInSwift5V' | %FileCheck -check-prefix=CHECK-NESTED-ALIAS-UNDERLYING-4 %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 5 -find-mangled '$sSo13InnerInSwift5V' | %FileCheck -check-prefix=CHECK-NESTED-ALIAS-UNDERLYING-5 %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled '$sSC16ImportantCStructa' | %FileCheck -check-prefix=CHECK-TOP-ALIAS-SYNTHESIZED-4 %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 5 -find-mangled '$sSC16ImportantCStructa' | %FileCheck -check-prefix=CHECK-TOP-ALIAS-SYNTHESIZED-5 %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled '$sSC13InnerInSwift5a' | %FileCheck -check-prefix=CHECK-NESTED-ALIAS-SYNTHESIZED-4 %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 5 -find-mangled '$sSC13InnerInSwift5a' | %FileCheck -check-prefix=CHECK-NESTED-ALIAS-SYNTHESIZED-5 %s

import APINotesFrameworkTest

// CHECK-TOP-ALIAS-UNDERLYING-4: struct VeryImportantCStruct {
// CHECK-TOP-ALIAS-UNDERLYING-5: struct VeryImportantCStruct {
// CHECK-NESTED-ALIAS-UNDERLYING-4: struct Inner {
// CHECK-NESTED-ALIAS-UNDERLYING-5: struct Inner {
// CHECK-TOP-ALIAS-SYNTHESIZED-4: typealias ImportantCStruct = VeryImportantCStruct
// CHECK-TOP-ALIAS-SYNTHESIZED-5: typealias ImportantCStruct = VeryImportantCStruct
// CHECK-NESTED-ALIAS-SYNTHESIZED-4: typealias InnerInSwift5 = Outer.Inner
// CHECK-NESTED-ALIAS-SYNTHESIZED-5: typealias InnerInSwift5 = Outer.Inner
