//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftDiagnostics
import SwiftParser
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

extension MacroExpansionContext {
  func diagnose(
    _ diag: TaskMacroDiagnostic,
    at node: some SyntaxProtocol
  ) {
    diagnose(Diagnostic(
      node: Syntax(node),
      message: diag
    ))
  }
}

enum TaskMacroDiagnostic: String, DiagnosticMessage {
  case noImplementation 
    = "'@Task' macro can only be used on functions with an implementation"
  case unsupportedGlobalActor
    = "'@Task' global actor must be written 'GlobalActorType'.shared"

  var message: String { rawValue }

  var severity: DiagnosticSeverity { .error }

  var diagnosticID: MessageID {
    MessageID(domain: "_Concurrency", id: "TaskMacro.\(self)")
  }
}


public struct TaskMacro: BodyMacro {
  public static func expansion(
    of node: AttributeSyntax,
    statements: CodeBlockItemListSyntax,
    in context: some MacroExpansionContext
  ) throws -> [CodeBlockItemSyntax] {
    var globalActor: TokenSyntax? = nil
    var argumentList: LabeledExprListSyntax = []
    if case .argumentList(let arguments) = node.arguments {
      if let actor = arguments.first(labeled: "on") {
        guard let member = actor.expression.as(MemberAccessExprSyntax.self),
              let declRef = member.base?.as(DeclReferenceExprSyntax.self) else {
          context.diagnose(.unsupportedGlobalActor, at: actor)
          return []
        }

        argumentList = LabeledExprListSyntax(arguments.dropFirst())
        globalActor = declRef.baseName
      } else {
        argumentList = arguments
      }
    }

    let signature: ClosureSignatureSyntax? =
      if let globalActor {
        .init(attributes: "@\(globalActor) ")
      } else {
        nil
      }

    let parens: (left: TokenSyntax, right: TokenSyntax)? =
      if !argumentList.isEmpty {
        (.leftParenToken(), .rightParenToken())
      } else {
        nil
      }

    let taskInit = FunctionCallExprSyntax(
      calledExpression: DeclReferenceExprSyntax(
        baseName: "Task"
      ),
      leftParen: parens?.left,
      arguments: argumentList,
      rightParen: parens?.right,
      trailingClosure: ClosureExprSyntax(
        signature: signature,
        statements: statements
      )
    )

    return ["\(taskInit)"]
  }

  public static func expansion(
    of node: AttributeSyntax,
    providingBodyFor declaration: some DeclSyntaxProtocol & WithOptionalCodeBlockSyntax,
    in context: some MacroExpansionContext
  ) throws -> [CodeBlockItemSyntax] {
    guard let taskBody = declaration.body else {
      context.diagnose(.noImplementation, at: node)
      return []
    }

    return try expansion(
      of: node,
      statements: taskBody.statements,
      in: context)
  }

  public static func expansion(
    of node: AttributeSyntax,
    providingBodyFor closure: ClosureExprSyntax,
    in context: some MacroExpansionContext
  ) throws -> [CodeBlockItemSyntax] {
    try expansion(
      of: node,
      statements: closure.statements,
      in: context)
  }
}
