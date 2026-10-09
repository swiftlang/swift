// REQUIRES: swift_swift_parser

// Verify that scanning a macro defined by expanding another macro does not
// evaluate its definition before imports are resolved.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: touch %t/%target-library-name(MacroDefinition)
// RUN: %target-swift-frontend -scan-dependencies -module-name MacroUser \
// RUN:   -module-cache-path %t/clang-module-cache -swift-version 5 \
// RUN:   -external-plugin-path %t#%swift-plugin-server \
// RUN:   %t/main.swift -o %t/deps.json
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps.json MacroUser macroDependencies | %FileCheck %s

// CHECK: MacroDefinition

//--- main.swift
@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

@freestanding(expression) macro stringifySeven() -> (Int, String) = #stringify(7)
