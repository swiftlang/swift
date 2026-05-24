//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntax

extension Syntax {
  /// Return a copy of this syntax tree with all comments removed from trivia.
  func strippingComments() -> Syntax {
    let remover = CommentRemover()
    return remover.rewrite(self)
  }
}

private class CommentRemover: SyntaxRewriter {
  /// Whether the first token has been visited yet. The leading trivia of the
  /// first token is checked for a `.swiftinterface` header (identified by a
  /// `// swift-interface-format-version:` comment). When present, all
  /// `// swift-` prefixed comments in that trivia are preserved while other
  /// comments (license headers, doc comments, etc.) are stripped as usual.
  private var isFirstToken = true

  override func visit(_ token: TokenSyntax) -> TokenSyntax {
    let rawLeading: Trivia
    if isFirstToken {
      isFirstToken = false
      let isSwiftInterface = token.leadingTrivia.contains {
        if case .lineComment(let text) = $0 {
          return text.hasPrefix("// swift-interface-format-version:")
        }
        return false
      }
      rawLeading = stripComments(from: token.leadingTrivia,
                                 preserveInterfaceHeader: isSwiftInterface)
    } else {
      rawLeading = stripComments(from: token.leadingTrivia)
    }
    // After stripping comments and removing declarations, consecutive blank
    // lines can accumulate. Collapse them so at most one blank line remains.
    let newLeading = collapseExtraBlankLines(in: rawLeading)
    let newTrailing = stripComments(from: token.trailingTrivia)
    if newLeading == token.leadingTrivia && newTrailing == token.trailingTrivia {
      return token
    }
    return token
      .with(\.leadingTrivia, newLeading)
      .with(\.trailingTrivia, newTrailing)
  }

  private func stripComments(
    from trivia: Trivia,
    preserveInterfaceHeader: Bool = false
  ) -> Trivia {
    var pieces: [TriviaPiece] = []
    var skipNextNewline = false

    for piece in trivia {
      switch piece {
      case .lineComment(let text), .docLineComment(let text):
        if preserveInterfaceHeader && text.hasPrefix("// swift-") {
          skipNextNewline = false
          pieces.append(piece)
        } else {
          skipNextNewline = true
        }
      case .blockComment, .docBlockComment:
        continue
      case .newlines, .carriageReturns, .carriageReturnLineFeeds:
        if skipNextNewline {
          skipNextNewline = false
          continue
        }
        skipNextNewline = false
        pieces.append(piece)
      default:
        skipNextNewline = false
        pieces.append(piece)
      }
    }

    return Trivia(pieces: pieces)
  }

  /// Collapse runs of consecutive newlines so that at most one blank line
  /// (two newline characters) separates content.
  private func collapseExtraBlankLines(in trivia: Trivia) -> Trivia {
    var pieces: [TriviaPiece] = []
    var pendingNewlines = 0

    for piece in trivia {
      switch piece {
      case .newlines(let n):
        pendingNewlines += n
      case .carriageReturns(let n):
        pendingNewlines += n
      case .carriageReturnLineFeeds(let n):
        pendingNewlines += n
      default:
        if pendingNewlines > 0 {
          pieces.append(.newlines(min(pendingNewlines, 2)))
          pendingNewlines = 0
        }
        pieces.append(piece)
      }
    }

    if pendingNewlines > 0 {
      pieces.append(.newlines(min(pendingNewlines, 2)))
    }

    return Trivia(pieces: pieces)
  }
}
