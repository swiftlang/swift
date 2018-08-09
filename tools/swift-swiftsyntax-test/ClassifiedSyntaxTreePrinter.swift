import SwiftSyntax
import Foundation

extension SyntaxClassification {
  var tag: String {
    switch self {
    case .none: return ""
    case .keyword: return "kw"
    case .identifier: return ""
    case .typeIdentifier: return "type"
    case .dollarIdentifier: return "dollar"
    case .integerLiteral: return "int"
    case .floatingLiteral: return "float"
    case .stringLiteral: return "str"
    case .stringInterpolationAnchor: return "anchor"
    case .poundDirectiveKeyword: return "#kw"
    case .buildConfigId: return "#id"
    case .attribute: return "attr-builtin"
    case .objectLiteral: return "object-literal"
    case .editorPlaceholder: return "placeholder"
    case .lineComment: return "comment-line"
    case .blockComment: return "comment-block"
    case .docLineComment: return "doc-comment-line"
    case .docBlockComment: return "doc-comment-block"
    }
  }
}

class ClassifiedSyntaxTreePrinter: SyntaxVisitor {
  private let classifications: [TokenSyntax: SyntaxClassification]
  private var currentClassification = SyntaxClassification.none
  private var skipNextNewline = false
  private var result = ""

  // MARK: Public interface

  init(classifications: [TokenSyntax: SyntaxClassification]) {
    self.classifications = classifications
  }

  func print(tree: SourceFileSyntax) -> String {
    result = ""
    visit(tree)
    // Emit the last closing tag
    recordCurrentClassification(.none)
    return result
  }

  // MARK: Implementation

  /// Closes the current tag if it is different from the previous one and opens
  /// a tag with the specified ID.
  private func recordCurrentClassification(
    _ classification: SyntaxClassification
  ) {
    if currentClassification != classification {
      if currentClassification != .none {
        result += "</" + currentClassification.tag + ">"
      }
      if classification != .none {
        result += "<" + classification.tag + ">"
      }
    }
    currentClassification = classification
  }

  private func visit(_ piece: TriviaPiece) {
    let classification: SyntaxClassification
    switch piece {
    case .spaces, .tabs, .verticalTabs, .formfeeds:
      classification = .none
    case .newlines, .carriageReturns, .carriageReturnLineFeeds:
      if skipNextNewline {
        skipNextNewline = false
        return
      }
      classification = .none
    case .backticks:
      classification = .none
    case .lineComment(let text):
      // Don't print CHECK lines
      if text.hasPrefix("// CHECK") {
        skipNextNewline = true
        return
      }
      classification = .lineComment
    case .blockComment:
      classification = .blockComment
    case .docLineComment:
      classification = .docLineComment
    case .docBlockComment:
      classification = .docBlockComment
    case .garbageText:
      classification = .none
    }
    recordCurrentClassification(classification)
    piece.write(to: &result)
  }

  private func visit(_ trivia: Trivia) {
    for piece in trivia {
      visit(piece)
    }
  }

  override func visit(_ node: TokenSyntax) {
    visit(node.leadingTrivia)
    let classification = classifications[node] ?? SyntaxClassification.none
    recordCurrentClassification(classification)
    result += node.text
    visit(node.trailingTrivia)
  }
}
