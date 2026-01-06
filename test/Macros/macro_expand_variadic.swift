// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/variadic_macros.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5
// RUN: %target-build-swift -target %target-swift-5.9-abi-triple -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// This test needs a Swift 5.9 runtime or newer.
// UNSUPPORTED: back_deployment_runtime

@freestanding(expression) macro print<each Value>(_ value: repeat each Value) = #externalMacro(module: "MacroDefinition", type: "PrintMacro")

@freestanding(expression) macro Print<each Value>(_ value: repeat each Value) = #externalMacro(module: "MacroDefinition", type: "PrintMacro")

struct Print<each Value> {
  init() {}
}

func testAmbiguity() {
  let _ = Print<Int>()
}

func testIt() {
  // CHECK: hello
  // CHECK: [1, 2, 3, 4, 5]
  #print("hello", [1, 2, 3, 4, 5])

  // CHECK: hello
  // CHECK: [1, 2, 3, 4, 5]
  #print<String, [Int]>("hello", [1, 2, 3, 4, 5])

  // CHECK: world
  #print("world")
}

testIt()
