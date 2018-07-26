import SwiftSyntax
import Foundation

class ClassifiedSyntaxTreePrinter: SyntaxVisitor {
  private let classifications: [TokenSyntax: SyntaxClassification]
  private var currentTag = ""
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
    recordCurrentTag("")
    return result
  }

  // MARK: Implementation

  /// Closes the current tag if it is different from the previous one and opens
  /// a tag with the specified ID.
  private func recordCurrentTag(_ tag: String) {
    if currentTag != tag {
      if !currentTag.isEmpty {
        result += "</" + currentTag + ">"
      }
      if !tag.isEmpty {
        result += "<" + tag + ">"
      }
    }
    currentTag = tag
  }

  private func visit(_ piece: TriviaPiece) {
    let tag: String
    switch piece {
    case .spaces, .tabs, .verticalTabs, .formfeeds:
      tag = ""
    case .newlines, .carriageReturns, .carriageReturnLineFeeds:
      if skipNextNewline {
        skipNextNewline = false
        return
      }
      tag = ""
    case .backticks:
      tag = ""
    case .lineComment(let text):
      // Don't print CHECK lines
      if text.hasPrefix("// CHECK") {
        skipNextNewline = true
        return
      }
      tag = "comment-line"
    case .blockComment:
      tag = "comment-block"
    case .docLineComment:
      tag = "doc-comment-line"
    case .docBlockComment:
      tag = "doc-comment-block"
    case .garbageText:
      tag = ""
    }
    recordCurrentTag(tag)
    piece.write(to: &result)
  }

  private func visit(_ trivia: Trivia) {
    for piece in trivia {
      visit(piece)
    }
  }

  private func getTagForSyntaxClassification(
    _ classification: SyntaxClassification
  ) -> String {
    switch (classification) {
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
    }
  }

  override func visit(_ node: TokenSyntax) {
    visit(node.leadingTrivia)
    let classification = classifications[node] ?? SyntaxClassification.none
    recordCurrentTag(getTagForSyntaxClassification(classification))
    result += node.text
    visit(node.trailingTrivia)
  }
}
