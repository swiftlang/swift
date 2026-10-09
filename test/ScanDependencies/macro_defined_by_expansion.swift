// REQUIRES: swift_swift_parser

// A macro defined by expanding another macro names no plugin module itself,
// so the scanner must not evaluate that definition. Evaluating it type-checks
// the expansion, which needs imports the scanner never resolves.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: touch %t/%target-library-name(MacroDefinition)
// RUN: %target-swift-frontend -scan-dependencies -module-name MacroUser \
// RUN:   -module-cache-path %t/clang-module-cache -swift-version 5 \
// RUN:   -external-plugin-path %t#%swift-plugin-server \
// RUN:   %t/main.swift -o %t/deps.json
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps.json MacroUser macroDependencies | %FileCheck %s

// The '#externalMacro' definition still records its plugin.
// CHECK: MacroDefinition

//--- main.swift
@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

@freestanding(expression) macro stringifySeven() -> (Int, String) = #stringify(7)
