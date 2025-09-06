// REQUIRES: swift_swift_parser

/// Test loading dependencies that has macros.
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build macros.
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroOne) -module-name=MacroOne %t/macro-1.swift
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroTwo) -module-name=MacroTwo %t/macro-2.swift

// RUN: %target-swift-frontend -emit-module %t/test.swift -module-name Test -o %t/Test.swiftmodule \
// RUN:   -swift-version 5 -external-plugin-path %t#%swift-plugin-server -package-name Test \
// RUN:   -plugin-path %swift-plugin-dir -prefix-serialized-debugging-options \
// RUN:   -load-plugin-library %t/%target-library-name(MacroOne) \
// RUN:   -debug-prefix-map %t=/externalsearchpath \
// RUN:   -debug-prefix-map %swift-plugin-server=/externalserverpath \
// RUN:   -debug-prefix-map %swift-plugin-dir=/plugindir
// RUN: llvm-bcanalyzer -dump %t/Test.swiftmodule | %FileCheck %s

// CHECK: <PLUGIN_SEARCH_OPTION abbrevid=8 op0=1/> blob data = '/externalsearchpath#/externalserverpath'
// CHECK: <PLUGIN_SEARCH_OPTION abbrevid=8 op0=0/> blob data = '/plugindir'
// CHECK: <PLUGIN_SEARCH_OPTION abbrevid=8 op0=2/> blob data = '/externalsearchpath/{{(lib)?}}MacroOne

//--- macro-1.swift
import SwiftSyntax
@_spi(ExperimentalLanguageFeature) import SwiftSyntaxMacros

public struct AssertMacro: ExpressionMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = macro.arguments.first?.expression else {
      fatalError("boom")
    }

    return "assert(\(argument))"
  }
}

//--- macro-2.swift
import SwiftSyntax
@_spi(ExperimentalLanguageFeature) import SwiftSyntaxMacros

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

//--- test.swift
import Swift
@freestanding(expression) public macro assert(_: Bool) = #externalMacro(module: "MacroOne", type: "AssertMacro")
@freestanding(expression) public macro assert_2(_: Bool) = #externalMacro(module: "MacroOne", type: "AssertMacro")
@freestanding(expression) public macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroTwo", type: "StringifyMacro")
@freestanding(expression) public macro stringify_2<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroTwo", type: "StringifyMacro")
