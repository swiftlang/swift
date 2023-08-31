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

  var severity: DiagnosticSeverity {
    .error
  }
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
