// RUN: rm -rf %t && mkdir -p %t

// Use fake mangled names here with the name of the imported module.
// swift-ide-test isn't testing the full demangling algorithm.

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 3 -find-mangled _T0So16ImportantCStructa | %FileCheck -check-prefix=CHECK-TOP-ALIAS %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled _T0So16ImportantCStructa | %FileCheck -check-prefix=CHECK-TOP-ALIAS %s

// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 3 -find-mangled _T0So13InnerInSwift4a | %FileCheck -check-prefix=CHECK-NESTED-ALIAS %s
// RUN: %target-swift-ide-test -F %S/Inputs/custom-frameworks -print-ast-typechecked -source-filename %s -swift-version 4 -find-mangled _T0So13InnerInSwift4a | %FileCheck -check-prefix=CHECK-NESTED-ALIAS %s

import APINotesFrameworkTest

// CHECK-TOP-ALIAS: typealias ImportantCStruct = VeryImportantCStruct
// CHECK-NESTED-ALIAS: typealias InnerInSwift4 = Outer.Inner
