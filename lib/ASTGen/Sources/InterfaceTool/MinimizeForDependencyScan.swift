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
import SwiftBasicFormat

extension SyntaxProtocol {
  /// Produce a minimized text form of this syntax tree that retains only code
  /// that affects dependency scanning.
  ///
  /// In particular, this keeps only
  ///   - `import` statements
  ///   - Non-empty `#if` blocks that contain other relevant content
  ///   - `#if canImport(...)` conditions
  ///   - `#externalMacro(...)` definitions
  ///   - `// swift-*` header comments for swift interface files
  public func minimizeForDependencyScan() -> String {
    let minimized = DependencyScanSourceMinimizer().rewrite(self, detach: true)
    let stripped = MinimizerTriviaStripper().rewrite(minimized, detach: true)
    let formatted = BasicFormat().rewrite(stripped, detach: true)
    return formatted.description
  }
}

/// Rewrites a syntax tree into its dependency-scan-relevant subset.
private final class DependencyScanSourceMinimizer: SyntaxRewriter {
  override func visit(_ node: SourceFileSyntax) -> SourceFileSyntax {
    var result = super.visit(node)
    // Preserve `swift-*` header comments. We capture it here rather than visit
    // the tokens directly.
    result.leadingTrivia = headerTrivia(of: node)
    // Strip any shebang.
    result.shebang = nil
    return result
  }

  /// The `// swift-*` header comments of a swift interface file, as trivia with
  /// each comment on its own line.
  private func headerTrivia(of node: SourceFileSyntax) -> Trivia {
    let trivia = node.firstToken(viewMode: .sourceAccurate)?.leadingTrivia ?? []
    return Trivia(pieces: trivia.flatMap { piece -> [TriviaPiece] in
        guard case .lineComment(let text) = piece, text.hasPrefix("// swift-") else {
          return []
        }
        return [piece, .newlines(1)]
      })
  }

  override func visit(_ node: CodeBlockItemListSyntax) -> CodeBlockItemListSyntax {
    CodeBlockItemListSyntax(
      node.compactMap { item -> CodeBlockItemSyntax? in
        guard case .decl(let decl) = item.item, shouldKeep(decl) else {
          return nil
        }

        let rewritten = visit(item)

        // Strip `#if` if it is empty after rewriting.
        if case .decl(let rewrittenDecl) = rewritten.item,
           let ifConfig = rewrittenDecl.as(IfConfigDeclSyntax.self),
           !ifConfig.clauses.contains(where: { $0.hasCanImportCondition || !$0.hasEmptyBody }) {
          return nil
        }

        return rewritten
      })
  }

  /// Whether `decl` is relevant to dependency scanning. We assume `#if` should
  /// be kept here, but it may be removed if it is empty and contains no
  /// `canImport` checks.
  private func shouldKeep(_ decl: DeclSyntax) -> Bool {
    if decl.is(ImportDeclSyntax.self) || decl.is(IfConfigDeclSyntax.self) {
      return true
    }
    if let macroDecl = decl.as(MacroDeclSyntax.self) {
      return macroDecl.isExternalMacro
    }
    return false
  }
}

/// Utility to strip whitespace and comments, but preserve the header comment.
///
/// The result is not parseable and should be used with a formatter like
/// `BasicFormat` if parsing the result is needed.
///
/// Note: this cannot be a `BasicFormat` subclass, because `BasicFormat` makes
/// whitespace decisions that depend on the next (not yet stripped) token in
/// addition to the current (stripped) token. It could be combined into the
/// minimizer itself, but it is less efficient since it modifies to more tokens.
private final class MinimizerTriviaStripper: SyntaxRewriter {
  override func visit(_ token: TokenSyntax) -> TokenSyntax {
    if token.leadingTrivia.isEmpty && token.trailingTrivia.isEmpty {
      // This early exit preserves the node's identity, which avoids allocation.
      return token
    }
    // Strip whitespace and comments.
    return token.with(\.leadingTrivia, []).with(\.trailingTrivia, [])
  }

  override func visit(_ node: SourceFileSyntax) -> SourceFileSyntax {
    var result = super.visit(node)
    // Preserve `swift-*` header comments that would otherwise be stripped.
    result.leadingTrivia = node.leadingTrivia
    // Ensure a non-empty file ends with a newline.
    // FIXME: this should ideally be done by the formatter.
    if !result.statements.isEmpty, result.endOfFileToken.leadingTrivia.pieces.last?.isNewline != true {
      result.endOfFileToken.leadingTrivia = result.endOfFileToken.leadingTrivia  + .newlines(1)
    }
    return result
  }
}

/// Utility to find any `canImport` uses in an `#if` condition.
private final class CanImportFinder: SyntaxVisitor {
  var foundCanImport = false
  override func visit(_ node: FunctionCallExprSyntax) -> SyntaxVisitorContinueKind {
    if let callee = node.calledExpression.as(DeclReferenceExprSyntax.self), callee.baseName.text == "canImport" {
      foundCanImport = true
      return .skipChildren
    }
    return .visitChildren
  }
}

extension IfConfigClauseSyntax {
  var hasCanImportCondition: Bool {
    guard let condition else {
      return false
    }
    let finder = CanImportFinder(viewMode: .sourceAccurate)
    finder.walk(condition)
    return finder.foundCanImport
  }

  var hasEmptyBody: Bool {
    switch elements {
      case .statements(let statements): return statements.isEmpty
      case .decls(let decls): return decls.isEmpty
      case .switchCases(let cases): return cases.isEmpty
      case .postfixExpression(_): return false
      case .attributes(let attrs): return attrs.isEmpty
      case nil: return true
    }
  }
}

extension MacroDeclSyntax {
  var isExternalMacro: Bool {
    if let expansion = definition?.value.as(MacroExpansionExprSyntax.self) {
      return expansion.macroName.text == "externalMacro"
    }
    return false
  }
}
