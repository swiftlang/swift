// REQUIRES: swift_swift_parser

// sandbox-exec is only avaiable in Darwin
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plugins)

// RUN: split-file %s %t

//== Build the plugins
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library \
// RUN:   -o %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -module-name=MacroDefinition \
// RUN:   %t/MacroDefinition.swift \
// RUN:   -g -no-toolchain-stdlib-rpath

// RUN: %swift-build-c-plugin -o %t/mock-plugin %t/TestPlugin.c

//== Nested sandbox. Expected to fail because sandbox-exec doesn't support nested sandboxing.
// RUN: not sandbox-exec -p '(version 1)(allow default)' \
// RUN:   %target-swift-frontend \
// RUN:   -typecheck -verify \
// RUN:   -swift-version 5 \
// RUN:   -external-plugin-path %t/plugins#%swift-plugin-server \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift

//== Avoid nested sandbox by -disable-sandbox
// RUN: sandbox-exec -p '(version 1)(allow default)' \
// RUN:   %target-swift-frontend \
// RUN:   -disable-sandbox \
// RUN:   -typecheck -verify \
// RUN:   -swift-version 5 \
// RUN:   -external-plugin-path %t/plugins#%swift-plugin-server \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift


//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct StringifyMacro: ExpressionMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = macro.arguments.first?.expression else {
      fatalError("boom")
    }

    return "(\(argument), \(StringLiteralExprSyntax(content: argument.description)))"
  }
}

//--- TestPlugin.c
#include "swift-c/MockPlugin/MockPlugin.h"

MOCK_PLUGIN([
  {
    "expect": {"getCapability": {}},
    "response": {"getCapabilityResult": {"capability": {"protocolVersion": 1}}}
  },
  {
    "expect": {"expandFreestandingMacro": {
                "macro": {"moduleName": "TestPlugin", "typeName": "TestStringMacro"}}},
    "response": {"expandMacroResult": {"expandedSource": "\"test string\"", "diagnostics": []}}
  }
])

//--- test.swift
@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")
@freestanding(expression) macro testString() -> String = #externalMacro(module: "TestPlugin", type: "TestStringMacro")

func test() {
  let _: String = #stringify(42).1
  let _: String = #testString
}
