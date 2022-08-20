// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weaklinked_import_helper.swiftmodule -parse-as-library %S/Inputs/weaklinked_import_helper.swift -enable-library-evolution
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Intermediate.swiftmodule -parse-as-library %t/Intermediate.swift -I %t -enable-library-evolution
//
// RUN: %target-swift-frontend -primary-file %t/Client.swift -I %t -emit-ir | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

//--- Intermediate.swift

import weaklinked_import_helper

public func hasDefaultArgument(_ s: S = S()) { }

@_alwaysEmitIntoClient
public func aeicFuncCallingFn() {
  fn()
}

//--- Client.swift

import Intermediate
@_weakLinked import weaklinked_import_helper

// Symbols from `weaklinked_import_helper` should have weak linkage even
// when the references to them are inlined from `Intermediate`, which imported
// `weaklinked_import_helper` without `@_weakLinked`.

func testDefaultArguments() {
  // CHECK: declare extern_weak {{.+}} @"$s24weaklinked_import_helper1SVMa"
  hasDefaultArgument()
}

func testAlwaysEmitIntoClient() {
  // CHECK-DAG: declare extern_weak {{.+}} @"$s24weaklinked_import_helper2fnyyF"()
  aeicFuncCallingFn()
}
