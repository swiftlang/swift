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

extension SyntaxProtocol {
  /// Produce a copy of this syntax node that retains only import statements
  /// and `#if` blocks that contain imports or `canImport()` conditions.
  ///
  /// This is useful for dependency scanning where only the import graph is
  /// needed, not the full public API surface.
  public func minimizedToImportsOnly() -> Syntax {
    let rewriter = ImportsOnlyMinimizer()
    return rewriter.rewrite(Syntax(self)).strippingComments()
  }
}

/// A `SyntaxRewriter` that strips everything except import statements and
/// `#if` blocks that contain imports or `canImport()` conditions.
class ImportsOnlyMinimizer: SyntaxRewriter {

  private enum Disposition {
    case keep
    case remove
  }

  // MARK: - List-level filtering

  override func visit(_ node: CodeBlockItemListSyntax) -> CodeBlockItemListSyntax {
    return filterCodeBlockItems(node)
  }

  override func visit(_ node: MemberBlockItemListSyntax) -> MemberBlockItemListSyntax {
    return filterMemberBlockItems(node)
  }

  private func filterCodeBlockItems(_ items: CodeBlockItemListSyntax) -> CodeBlockItemListSyntax {
    var newItems: [CodeBlockItemSyntax] = []
    var anyChanged = false

    for index in items.indices {
      let item = items[index]

      guard case .decl(let decl) = item.item else {
        if anyChanged {
          newItems.append(item)
        }
        continue
      }

      let disposition = declarationDisposition(decl)
      switch disposition {
      case .keep:
        let rewritten = rewrite(Syntax(item)).as(CodeBlockItemSyntax.self) ?? item
        if rewritten.id != item.id {
          noteChanged(&anyChanged, &newItems, items, index)
          newItems.append(rewritten)
        } else if anyChanged {
          newItems.append(item)
        }
      case .remove:
        noteChanged(&anyChanged, &newItems, items, index)
      }
    }

    if !anyChanged {
      return items
    }
    return CodeBlockItemListSyntax(newItems)
  }

  private func filterMemberBlockItems(_ items: MemberBlockItemListSyntax) -> MemberBlockItemListSyntax {
    var newItems: [MemberBlockItemSyntax] = []
    var anyChanged = false

    for index in items.indices {
      let item = items[index]
      let decl = item.decl

      let disposition = declarationDisposition(DeclSyntax(decl))
      switch disposition {
      case .keep:
        let rewritten = rewrite(Syntax(item)).as(MemberBlockItemSyntax.self) ?? item
        if rewritten.id != item.id {
          noteChanged(&anyChanged, &newItems, items, index)
          newItems.append(rewritten)
        } else if anyChanged {
          newItems.append(item)
        }
      case .remove:
        noteChanged(&anyChanged, &newItems, items, index)
      }
    }

    if !anyChanged {
      return items
    }
    return MemberBlockItemListSyntax(newItems)
  }

  private func noteChanged<C: SyntaxCollection>(
    _ anyChanged: inout Bool,
    _ newItems: inout [C.Element],
    _ collection: C,
    _ currentIndex: C.Index
  ) {
    if anyChanged { return }
    anyChanged = true
    newItems.append(contentsOf: collection[..<currentIndex])
  }

  // MARK: - Declaration disposition

  private func declarationDisposition(_ decl: DeclSyntax) -> Disposition {
    if decl.is(ImportDeclSyntax.self) {
      return .keep
    }

    if let ifConfig = decl.as(IfConfigDeclSyntax.self) {
      return ifConfigDisposition(ifConfig)
    }

    return .remove
  }

  // MARK: - #if block handling

  private func ifConfigDisposition(_ node: IfConfigDeclSyntax) -> Disposition {
    // If any clause condition contains canImport(), keep the entire block.
    for clause in node.clauses {
      if let condition = clause.condition,
        exprContainsCanImport(ExprSyntax(condition))
      {
        return .keep
      }
    }

    // Otherwise, check if any clause body contains imports or qualifying
    // nested #if blocks.
    for clause in node.clauses {
      if clauseContainsImports(clause) {
        return .keep
      }
    }

    return .remove
  }

  /// Intercept `#if` blocks to filter clause bodies, keeping only imports
  /// and qualifying nested `#if` blocks.
  override func visit(_ node: IfConfigDeclSyntax) -> DeclSyntax {
    // Filter each clause body to keep only imports and qualifying nested #if.
    var newClauses: [IfConfigClauseSyntax] = []
    var anyChanged = false

    for clause in node.clauses {
      let filtered = filterClauseBody(clause)
      if filtered.id != clause.id {
        anyChanged = true
      }
      newClauses.append(filtered)
    }

    if anyChanged {
      return DeclSyntax(node.with(\.clauses, IfConfigClauseListSyntax(newClauses)))
    }
    return DeclSyntax(node)
  }

  // MARK: - Clause body filtering

  private func filterClauseBody(_ clause: IfConfigClauseSyntax) -> IfConfigClauseSyntax {
    guard let elements = clause.elements else {
      return clause
    }

    switch elements {
    case .statements(let stmts):
      let filtered = filterClauseCodeBlockItems(stmts)
      if filtered.id != stmts.id {
        return clause.with(\.elements, .statements(filtered))
      }
      return clause

    case .decls(let members):
      let filtered = filterClauseMemberBlockItems(members)
      if filtered.id != members.id {
        return clause.with(\.elements, .decls(filtered))
      }
      return clause

    default:
      return clause
    }
  }

  private func filterClauseCodeBlockItems(
    _ items: CodeBlockItemListSyntax
  ) -> CodeBlockItemListSyntax {
    var newItems: [CodeBlockItemSyntax] = []
    var anyChanged = false

    for index in items.indices {
      let item = items[index]

      guard case .decl(let decl) = item.item else {
        noteChanged(&anyChanged, &newItems, items, index)
        continue
      }

      let disposition = declarationDisposition(decl)
      switch disposition {
      case .keep:
        if anyChanged {
          newItems.append(item)
        }
      case .remove:
        noteChanged(&anyChanged, &newItems, items, index)
      }
    }

    if !anyChanged {
      return items
    }
    return CodeBlockItemListSyntax(newItems)
  }

  private func filterClauseMemberBlockItems(
    _ items: MemberBlockItemListSyntax
  ) -> MemberBlockItemListSyntax {
    var newItems: [MemberBlockItemSyntax] = []
    var anyChanged = false

    for index in items.indices {
      let item = items[index]
      let decl = item.decl

      let disposition = declarationDisposition(DeclSyntax(decl))
      switch disposition {
      case .keep:
        if anyChanged {
          newItems.append(item)
        }
      case .remove:
        noteChanged(&anyChanged, &newItems, items, index)
      }
    }

    if !anyChanged {
      return items
    }
    return MemberBlockItemListSyntax(newItems)
  }

  // MARK: - canImport detection

  /// Recursively check if an expression contains a `canImport(...)` call.
  private func exprContainsCanImport(_ expr: ExprSyntax) -> Bool {
    if let call = expr.as(FunctionCallExprSyntax.self),
      let ref = call.calledExpression.as(DeclReferenceExprSyntax.self),
      ref.baseName.text == "canImport"
    {
      return true
    }

    if let infix = expr.as(InfixOperatorExprSyntax.self) {
      return exprContainsCanImport(infix.leftOperand)
        || exprContainsCanImport(infix.rightOperand)
    }

    if let prefix = expr.as(PrefixOperatorExprSyntax.self) {
      return exprContainsCanImport(prefix.expression)
    }

    if let tuple = expr.as(TupleExprSyntax.self) {
      return tuple.elements.contains { exprContainsCanImport($0.expression) }
    }

    if let sequence = expr.as(SequenceExprSyntax.self) {
      return sequence.elements.contains { exprContainsCanImport($0) }
    }

    return false
  }

  // MARK: - Clause import detection

  /// Check if a clause body contains any import declarations or qualifying
  /// nested `#if` blocks.
  private func clauseContainsImports(_ clause: IfConfigClauseSyntax) -> Bool {
    guard let elements = clause.elements else {
      return false
    }

    switch elements {
    case .statements(let stmts):
      return stmts.contains { item in
        guard case .decl(let decl) = item.item else { return false }
        return declarationDisposition(decl) == .keep
      }
    case .decls(let members):
      return members.contains { item in
        declarationDisposition(DeclSyntax(item.decl)) == .keep
      }
    default:
      return false
    }
  }
}
