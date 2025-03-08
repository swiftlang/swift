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

struct TaskMacroDiagnostic: DiagnosticMessage {
  static func diagnose(at node: some SyntaxProtocol) -> Diagnostic {
    Diagnostic(node: Syntax(node), message: Self.init())
  }

  var message: String {
    "'@StartTask' macro can only be used on functions with an implementation"
  }

  var severity: DiagnosticSeverity { .error }

  var diagnosticID: MessageID {
    MessageID(domain: "_Concurrency", id: "StartMacro.\(self)")
  }
}


public struct StartTaskMacro: BodyMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingBodyFor declaration: some DeclSyntaxProtocol & WithOptionalCodeBlockSyntax,
    in context: some MacroExpansionContext
  ) throws -> [CodeBlockItemSyntax] {
    guard let taskBody = declaration.body else {
      context.diagnose(TaskMacroDiagnostic.diagnose(at: node))
      return []
    }

    return [
      """
      Task \(taskBody)
      """
    ]
  }

  public static func expansion(
    of node: AttributeSyntax,
    providingBodyFor closure: ClosureExprSyntax,
    in context: some MacroExpansionContext
  ) throws -> [CodeBlockItemSyntax] {
    return [
      """
      Task {
      \(closure.statements)
      }
      """
    ]
  }
}
