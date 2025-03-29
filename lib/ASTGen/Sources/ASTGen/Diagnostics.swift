//===--- Diagnostics.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftDiagnostics
import SwiftSyntax

//===----------------------------------------------------------------------===//
// MARK: Diagnostic infrastructures
//===----------------------------------------------------------------------===//

extension ASTGenVisitor {
  /// Emits the given ASTGen diagnostic via the C++ diagnostic engine.
  func diagnose(_ diag: ASTGenDiagnostic) {
    self.diagnose(Diagnostic(
      node: diag.node,
      position: diag.position,
      message: diag,
      highlights: diag.highlights,
      notes: diag.notes.map { Note(node: $0.node, message: $0) },
      fixIts: diag.fixIts.map { FixIt(message: $0, changes: $0.changes) }
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
  struct Note: NoteMessage {
    var node: Syntax
    var message: String
    var noteID: MessageID

    init(node: Syntax, message: String, messageID: String) {
      self.node = node
      self.message = message
      self.noteID = MessageID(domain: "ASTGen", id: messageID)
    }
  }

  struct FixIt: FixItMessage {
    typealias Change = SwiftDiagnostics.FixIt.Change

    var message: String
    var fixItID: MessageID
    var changes: [Change]

    init(message: String, messageID: String, changes: [Change]) {
      self.fixItID = MessageID(domain: "ASTGen", id: messageID)
      self.message = message
      self.changes = changes
    }
  }

  var node: Syntax
  var position: AbsolutePosition?
  var message: String
  var severity: DiagnosticSeverity
  var messageID: String
  var highlights: [Syntax] = []
  var notes: [Note] = []
  var fixIts: [FixIt] = []

  var diagnosticID: MessageID {
    MessageID(domain: "ASTGen", id: messageID)
  }

  init(id: String, node: Syntax, position: AbsolutePosition? = nil, message: String, severity: DiagnosticSeverity) {
    self.node = node
    self.position = position
    self.message = message
    self.severity = severity
    self.messageID = id
  }
}

// Convenient methods.
extension FixIt.Change {
  static func replace(_ oldNode: some SyntaxProtocol, with newNode: some SyntaxProtocol) -> Self {
    .replace(oldNode: Syntax(oldNode), newNode: Syntax(newNode))
  }

  static func replaceTokenText(_ token: TokenSyntax, with tokenKind: TokenKind) -> Self {
    .replace(oldNode: Syntax(token), newNode: Syntax(token.detached.with(\.tokenKind, tokenKind)))
  }
}
