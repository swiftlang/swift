// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/variadic_macros.swift -g -no-toolchain-stdlib-rpath
// RUNx: %target-swift-frontend -dump-ast -enable-experimental-feature Macros -enable-experimental-feature VariadicGenerics -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -module-name MacroUser 2>&1 | %FileCheck --check-prefix CHECK-AST %s
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-experimental-feature Macros -enable-experimental-feature VariadicGenerics -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5
// RUN: %target-build-swift -swift-version 5 -enable-experimental-feature Macros -enable-experimental-feature VariadicGenerics -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@freestanding(expression) macro print<Value...>(_ value: repeat each Value) = #externalMacro(module: "MacroDefinition", type: "PrintMacro")

func testIt() {
  // CHECK: hello
  // CHECK: [1, 2, 3, 4, 5]
  #print("hello", [1, 2, 3, 4, 5])

  // CHECK: world
  #print("world")
}

testIt()
