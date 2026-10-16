// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macro.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift

// A closure with a capture list used as a macro argument must type-check:
// the capture list's pattern bindings belong to the expansion buffer being
// checked and used to be skipped as macro-generated declarations, leaving
// their initializers unresolved and failing with "failed to produce
// diagnostic for expression".
// https://github.com/swiftlang/swift/issues/86871
// https://github.com/swiftlang/swift/issues/80050

//--- macro.swift

import SwiftSyntax
import SwiftSyntaxMacros

public struct CallClosureMacro: ExpressionMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    guard let closure = node.trailingClosure else {
      fatalError("#callClosure requires a trailing closure")
    }
    return "(\(closure.trimmed))()"
  }
}

public struct WrapMacro: ExpressionMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    guard let closure = node.trailingClosure else {
      fatalError("#wrap requires a trailing closure")
    }
    return "wrap(\(closure.trimmed))"
  }
}

public struct NoOpBodyMacro: BodyMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingBodyFor declaration: some DeclSyntaxProtocol & WithOptionalCodeBlockSyntax,
    in context: some MacroExpansionContext
  ) throws -> [CodeBlockItemSyntax] {
    declaration.body.map { Array($0.statements) } ?? []
  }
}

//--- main.swift

@freestanding(expression)
macro callClosure(_ body: () -> Void) = #externalMacro(module: "MacroDefinition", type: "CallClosureMacro")

func wrap<T>(_ body: @escaping () -> T) -> () -> T { body }

@freestanding(expression)
macro wrap<T>(_ body: @escaping () -> T) -> () -> T = #externalMacro(module: "MacroDefinition", type: "WrapMacro")

@attached(body)
macro NoOpBody() = #externalMacro(module: "MacroDefinition", type: "NoOpBodyMacro")

func shorthandCaptureOfLocal() {
  var x = 1
  #callClosure { [x] in _ = x }
  x += 1
  _ = x
}

func explicitInitializerCapture() {
  let x = 2
  #callClosure { [y = x] in _ = y }
}

func parameterCapture(value: Int) {
  #callClosure { [value] in _ = value }
}

final class Object {
  func weakCapture() {
    #callClosure { [weak self] in _ = self }
  }
}

func bodyOnlyReference() {
  let x = 4
  #callClosure { _ = x }
}

func nestedClosureCapture() {
  let x = 5
  #callClosure {
    let inner = { [x] in _ = x }
    inner()
  }
}

// https://github.com/swiftlang/swift/issues/86871
func genericWrap() {
  let y = 42
  _ = wrap({ [y] in y })
  _ = #wrap { [y] in y }
}

// https://github.com/swiftlang/swift/issues/80050
class Demo {
  @NoOpBody
  func demo() {
    let closure = { [weak self] in
      _ = self
    }
    _ = closure
  }
}
