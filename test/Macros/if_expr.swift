// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: not %target-swift-frontend -typecheck %s -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -serialize-diagnostics-path %t/diags.dia

// RUN: c-index-test -read-diagnostics %t/diags.dia 2>&1 | %FileCheck -check-prefix DIAGS %s

// REQUIRES: swift_swift_parser

@attached(member, names: named(bar))
macro TestMacro<T>(_ x: T) = #externalMacro(module: "MacroDefinition", type: "InvalidIfExprMacro")

// Make sure we diagnose both the use in the custom attribute and in the expansion.
// DIAGS-DAG: if_expr.swift:[[@LINE+1]]:12: error: 'if' may only be used as expression in return, throw, or as the source of an assignment
@TestMacro(if .random() { 0 } else { 0 })
struct S {}

// DIAGS-DAG: @__swiftmacro_7if_expr1S9TestMacrofMm_.swift:2:12: error: 'if' may only be used as expression in return, throw, or as the source of an assignment
