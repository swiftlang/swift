// REQUIRES: swift_swift_parser, executable_test, string_processing

// RUN: %empty-directory(%t)

// Build the plugin
// RUN: %host-build-swift %S/../../Macros/Inputs/syntax_macro_definitions.swift -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition -swift-version 5 -g -no-toolchain-stdlib-rpath

// RUN: %target-build-swift %s -module-name Macro -emit-module -emit-module-path %t -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %target-swift-symbolgraph-extract -module-name Macro -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Macro.symbols.json
// RUN: %FileCheck %s --input-file %t/Macro.symbols.json --check-prefix MISSING

// Also check for a macro expansion.
// RUN: %empty-directory(%t/sg)
// RUN: %target-swift-frontend %s -module-name Macro -emit-module -emit-module-path %t/sg/Macro.swiftmodule -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -emit-symbol-graph -emit-symbol-graph-dir %t/sg
// RUN: %FileCheck %s --check-prefix EXPANDED --input-file %t/sg/Macro.symbols.json

@freestanding(expression)
public macro customFileID() -> String = #externalMacro(module: "MacroDefinition", type: "FileIDMacro")

@attached(peer)
public macro addCompletionHandler() = #externalMacro(module: "MacroDefinition", type: "AddCompletionHandler")

@freestanding(expression)
macro moduleCustomFileID() -> String = #externalMacro(module: "MacroDefinition", type: "FileIDMacro")

// CHECK-DAG: "precise": "s:5Macro20addCompletionHandleryycfm"
// CHECK-DAG: "precise": "s:5Macro12customFileIDSSycfm"

// MISSING-NOT: moduleCustomFileID

public protocol MyProtocol {}

@attached(extension, conformances: MyProtocol)
public macro AddMyProtocol() = #externalMacro(module: "MacroDefinition", type: "ConformanceViaExtensionMacro")

@attached(extension, names: named(Nested))
public macro AddNestedType() = #externalMacro(module: "MacroDefinition", type: "NestedConformingExtensionMacro")

@AddNestedType
public struct Outer {}

// EXPANDED-COUNT-1: "precise":"s:5Macro5OuterV6NestedV"
// EXPANDED-NOT: "precise":"s:5Macro5OuterV6NestedV"
