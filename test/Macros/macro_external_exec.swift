// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -emit-library-path=%t/%target-library-name(MacroDefinition) -working-directory=%t -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift
// RUN: %target-build-swift -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

macro customFileID: String = MacroDefinition.FileIDMacro

func testFunc(a: Int, b: Int) {
  // CHECK: MacroUser/macro_external_exec.swift
  print("Result is \(#customFileID)")
}

testFunc(a: 1, b: 2)
