// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

@freestanding(expression) public macro multiStatement() -> Int = #externalMacro(module: "MacroDefinition", type: "MultiStatementClosure")

func multiStatementInference() -> Int {
  #multiStatement()
}

// CHECK: 10
print(multiStatementInference())
