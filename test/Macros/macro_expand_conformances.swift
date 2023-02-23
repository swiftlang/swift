// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUNx: %target-swift-frontend -dump-macro-expansions -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -module-name MacroUser 2>&1 | %FileCheck --check-prefix CHECK-DUMP %s
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5
// RUN: %target-build-swift -swift-version 5 -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@attached(conformance)
macro Equatable() = #externalMacro(module: "MacroDefinition", type: "EquatableMacro")

func requireEquatable(_ value: some Equatable) {
  print(value == value)
}

@Equatable
struct S {}

// CHECK-DUMP: extension S : Equatable  {}

// CHECK: true
requireEquatable(S())
