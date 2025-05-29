// REQUIRES: swift_swift_parser

/// Test loading dependencies that has macros.
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build macros.
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroOne) -module-name=MacroOne %t/macro-1.swift
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroTwo) -module-name=MacroTwo %t/macro-2.swift

// RUN: %target-swift-frontend -emit-module %t/test.swift -module-name Test -o %t/Test.swiftmodule \
// RUN:   -swift-version 5 -external-plugin-path %t#%swift-plugin-server
// RUN: llvm-bcanalyzer -dump %t/Test.swiftmodule | %FileCheck %s

// CHECK-COUNT-1: <EXTERNAL_MACRO abbrevid=13 op0=4/> blob data = 'MacroOne'
// CHECK-COUNT-1: <EXTERNAL_MACRO abbrevid=13 op0=4/> blob data = 'MacroTwo'

// RUN: %target-swift-frontend -emit-module %t/test2.swift -module-name Test2 -o %t/Test2.swiftmodule \
// RUN:   -swift-version 5 -external-plugin-path %t#%swift-plugin-server -package-name Test
// RUN: llvm-bcanalyzer -dump %t/Test2.swiftmodule | %FileCheck %s --check-prefix CHECK2

// CHECK2-COUNT-1: <EXTERNAL_MACRO abbrevid=13 op0=4/> blob data = 'MacroOne'
// CHECK2-COUNT-1: <EXTERNAL_MACRO abbrevid=13 op0=3/> blob data = 'MacroTwo'

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

//--- test2.swift
import Swift
@freestanding(expression) public macro assert(_: Bool) = #externalMacro(module: "MacroOne", type: "AssertMacro")
@freestanding(expression) package macro assertPackage(_: Bool) = #externalMacro(module: "MacroOne", type: "AssertMacro")
@freestanding(expression) macro assertInternal(_: Bool) = #externalMacro(module: "MacroOne", type: "AssertMacro")

@freestanding(expression) macro stringifyPackage<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroTwo", type: "StringifyMacro")
@freestanding(expression) package macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroTwo", type: "StringifyMacro")
