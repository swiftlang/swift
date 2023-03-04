// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)

// Build the plugin
// RUN: %host-build-swift %S/../../Macros/Inputs/syntax_macro_definitions.swift -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition -swift-version 5 -g -no-toolchain-stdlib-rpath

// RUN: %target-build-swift %s -module-name Macro -emit-module -emit-module-path %t -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %target-swift-symbolgraph-extract -module-name Macro -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Macro.symbols.json
// RUN: %FileCheck %s --input-file %t/Macro.symbols.json --check-prefix MISSING

@freestanding(expression)
public macro customFileID() -> String = #externalMacro(module: "MacroDefinition", type: "FileIDMacro")

@attached(peer)
public macro addCompletionHandler() = #externalMacro(module: "MacroDefinition", type: "AddCompletionHandler")

@freestanding(expression)
macro moduleCustomFileID() -> String = #externalMacro(module: "MacroDefinition", type: "FileIDMacro")

// CHECK-DAG: "precise": "s:5Macro20addCompletionHandleryycfm"
// CHECK-DAG: "precise": "s:5Macro12customFileIDSSycfm"

// MISSING-NOT: moduleCustomFileID
