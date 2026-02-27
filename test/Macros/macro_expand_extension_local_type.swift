// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Verify that emitting IR with debug info does not crash when an extension
// macro generates a function containing a local type.
// RUN: %target-swift-frontend -swift-version 5 -emit-ir -load-plugin-library %t/%target-library-name(MacroDefinition) %s -module-name MacroUser -o - -g | %FileCheck %s

@attached(extension, names: named(test))
macro ExtensionWithLocalType() = #externalMacro(module: "MacroDefinition", type: "ExtensionWithLocalTypeMacro")

@ExtensionWithLocalType
struct MyStruct {}

// CHECK: define {{.*}}test
