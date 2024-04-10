//===----------------------------------------------------------------------===//
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

protocol ASTGenError: DiagnosticMessage {}

extension ASTGenError {
  var diagnosticID: MessageID {
    MessageID(domain: "ASTGen", id: "\(Self.self)")
  }

  var severity: DiagnosticSeverity { .error }
}

/// An error emitted when a token is of an unexpected kind.
struct UnexpectedTokenKindError: ASTGenError {
  let token: TokenSyntax
  private let parent: Syntax

  init(token: TokenSyntax) {
    guard let parent = token.parent else {
      preconditionFailure("Expected a child (not a root) token")
    }

    self.token = token
    self.parent = parent
  }

  var message: String {
    return """
      unexpected token kind for token:
        \(self.token.debugDescription)
      in parent:
        \(self.parent.debugDescription(indentString: "  "))
      """
  }
}

/// An error emitted when an optional child token is unexpectedly nil.
struct MissingChildTokenError: ASTGenError {
  let parent: Syntax
  let kindOfTokenMissing: TokenKind

  init(parent: some SyntaxProtocol, kindOfTokenMissing: TokenKind) {
    self.parent = Syntax(parent)
    self.kindOfTokenMissing = kindOfTokenMissing
  }

  var message: String {
    """
    missing child token of kind '\(self.kindOfTokenMissing)' in:
      \(parent.debugDescription(indentString: "  "))
    """
  }
}

/// An error emitted when a syntax collection entry is encountered that is considered a duplicate of a previous entry
/// per the language grammar.
struct DuplicateSyntaxError: ASTGenError {
  let duplicate: Syntax
  let original: Syntax

  init(duplicate: some SyntaxProtocol, original: some SyntaxProtocol) {
    precondition(duplicate.kind == original.kind, "Expected duplicate and original to be of same kind")

    guard let duplicateParent = duplicate.parent, let originalParent = original.parent,
      duplicateParent == originalParent, duplicateParent.kind.isSyntaxCollection
    else {
      preconditionFailure("Expected a shared syntax collection parent")
    }

    self.duplicate = Syntax(duplicate)
    self.original = Syntax(original)
  }

  var message: String {
    """
    unexpected duplicate syntax in list:
      \(duplicate.debugDescription(indentString: "  "))
    previous syntax:
      \(original.debugDescription(indentString: "  "))
    """
  }
}

struct NonTrivialPatternForAccessorError: ASTGenError {
  var message: String {
    "getter/setter can only be defined for a single variable"
  }
}

struct UnknownAccessorSpecifierError: ASTGenError {
  var specifier: TokenSyntax
  init(_ specifier: TokenSyntax) {
    self.specifier = specifier
  }

  var message: String {
    "unknown accessor specifier '\(specifier.text)'"
  }
}
