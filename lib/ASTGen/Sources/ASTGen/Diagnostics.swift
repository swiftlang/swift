//===--- Diagnostics.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftDiagnostics
import SwiftSyntax

extension ASTGenVisitor {
  /// Emits the given ASTGen diagnostic via the C++ diagnostic engine.
  func diagnose(_ message: ASTGenDiagnostic, highlights: [Syntax]? = nil, notes: [Note] = [], fixIts: [FixIt] = []) {
    self.diagnose(Diagnostic(
      node: message.node,
      message: message,
      highlights: highlights,
      notes: notes,
      fixIts: fixIts
    ))
  }

  /// Emits the given diagnostic via the C++ diagnostic engine.
  func diagnose(_ diagnostic: Diagnostic) {
    emitDiagnostic(
      diagnosticEngine: self.diagnosticEngine,
      sourceFileBuffer: self.base,
      diagnostic: diagnostic,
      diagnosticSeverity: diagnostic.diagMessage.severity
    )
  }

  /// Emits the given diagnostics via the C++ diagnostic engine.
  func diagnoseAll(_ diagnostics: [Diagnostic]) {
    diagnostics.forEach(diagnose)
  }
}

struct ASTGenDiagnostic: DiagnosticMessage {
  var node: Syntax
  var message: String
  var severity: DiagnosticSeverity
  var messageID: String

  var diagnosticID: MessageID {
    MessageID(domain: "ASTGen", id: messageID)
  }

  init(node: some SyntaxProtocol, message: String, severity: DiagnosticSeverity = .error, messageID: String) {
    self.node = Syntax(node)
    self.message = message
    self.severity = severity
    self.messageID = messageID
  }

  fileprivate init(node: some SyntaxProtocol, message: String, severity: DiagnosticSeverity = .error, function: String = #function) {
    // Extract messageID from the function name.
    let messageID = String(function.prefix(while: { $0 != "(" }))
    self.init(node: node, message: message, severity: severity, messageID: messageID)
  }
}

extension ASTGenDiagnostic {
  /// An error emitted when a token is of an unexpected kind.
  static func unexpectedTokenKind(token: TokenSyntax) -> Self {
    guard let parent = token.parent else {
      preconditionFailure("Expected a child (not a root) token")
    }

    return Self(
      node: token,
      message: """
      unexpected token kind for token:
        \(token.debugDescription)
      in parent:
        \(parent.debugDescription(indentString: "  "))
      """
    )
  }

  /// An error emitted when an optional child token is unexpectedly nil.
  static func missingChildToken(parent: some SyntaxProtocol, kindOfTokenMissing: TokenKind) -> Self {
    Self(
      node: parent,
      message: """
      missing child token of kind '\(kindOfTokenMissing)' in:
        \(parent.debugDescription(indentString: "  "))
      """
    )
  }

  /// An error emitted when a syntax collection entry is encountered that is
  /// considered a duplicate of a previous entry per the language grammar.
  static func duplicateSyntax(duplicate: some SyntaxProtocol, original: some SyntaxProtocol) -> Self {
    precondition(duplicate.kind == original.kind, "Expected duplicate and original to be of same kind")

    guard let duplicateParent = duplicate.parent, let originalParent = original.parent,
      duplicateParent == originalParent, duplicateParent.kind.isSyntaxCollection
    else {
      preconditionFailure("Expected a shared syntax collection parent")
    }

    return Self(
      node: duplicate,
      message: """
      unexpected duplicate syntax in list:
        \(duplicate.debugDescription(indentString: "  "))
      previous syntax:
        \(original.debugDescription(indentString: "  "))
      """
    )
  }

  static func nonTrivialPatternForAccessor(_ pattern: some SyntaxProtocol) -> Self {
    Self(
      node: pattern,
      message: "getter/setter can only be defined for a single variable"
    )
  }

  static func unknownAccessorSpecifier(_ specifier: TokenSyntax) -> Self {
    Self(
      node: specifier,
      message: "unknown accessor specifier '\(specifier.text)'"
    )
  }
}

/// Decl diagnostics
extension ASTGenDiagnostic {
  static func illegalTopLevelStmt(_ stmt: some SyntaxProtocol) -> Self {
    Self(
      node: stmt,
      message: "statements are not allowed at the top level"
    )
  }

  static func illegalTopLevelExpr(_ expr: some SyntaxProtocol) -> Self {
    Self(
      node: expr,
      message: "expressions are not allowed at the top level"
    )
  }

  static func invalidDefaultIsolationSpecifier(_ specifier: some SyntaxProtocol) -> Self {
    Self(
      node: specifier,
      message: "default isolation can only be set to '@MainActor' or 'nonisolated'"
    )
  }
}

/// DeclAttributes diagnostics
extension ASTGenDiagnostic {
  static func expectedArgumentsInAttribute(_ attribute: AttributeSyntax) -> Self {
    // FIXME: The diagnostic position should be at the and of the attribute name.
    Self(
      node: attribute,
      message: "expected arguments for '\(attribute.attributeName.trimmedDescription)' attribute"
    )
  }

  static func extraneousArgumentsInAttribute(_ attribute: AttributeSyntax, _ extra: some SyntaxProtocol) -> Self {
    Self(
      node: extra,
      message: "unexpected arguments in '\(attribute.attributeName.trimmedDescription)' attribute"
    )
  }
}

extension ASTGenDiagnostic {
  static func poundDiagnostic(_ node: StringLiteralExprSyntax, message: String, isError: Bool) -> Self {
    Self(
      node: node,
      message: node.representedLiteralValue!,
      severity: isError ? .error : .warning
    )
  }
}
