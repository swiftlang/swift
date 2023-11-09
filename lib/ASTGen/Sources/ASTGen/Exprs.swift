//===--- Exprs.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import SwiftDiagnostics
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

/// Check if an `ExprSyntax` can be generated using ASTGen.
///
/// If all the expression nodes that shares the first token are migrated,
/// returns true. For example, given
///   ```
///   foo.bar({ $0 + 1 }) + 2
///   ```
/// `foo` token is the first token of all `SequenceExpr`, `FunctionCallExpr`,
/// `MemberAccessExpr`, and `DeclReferenceExpr`. All these expression kinds must
/// be migrated to handle it in ASTGen. Because the fallback
/// `generateWithLegacy(_:)` only receives the parser position, it eagerly
/// parses the outer expressions.
func isExprMigrated(_ node: ExprSyntax) -> Bool {
  var current: Syntax = Syntax(node)
  if let firstToken = node.firstToken(viewMode: .sourceAccurate) {
    current = firstToken.parent!
  }
  while true {
    switch current.kind {
    case // Known implemented kinds.
        .closureExpr, .functionCallExpr, .declReferenceExpr, .memberAccessExpr,
        .tupleExpr, .ifExpr, .booleanLiteralExpr, .integerLiteralExpr,
        .arrayExpr, .nilLiteralExpr, .stringLiteralExpr:

      // `generate(_: StringLiteralExprSyntax)` doesn't support interpolations.
      if let str = current.as(StringLiteralExprSyntax.self) {
        if str.segments.count != 1 {
          return false
        }
        assert(str.segments.first!.is(StringSegmentSyntax.self))
      }

      // NOTE: When SequenceExpr is implemented, we need some special handling.
      // E.g.
      //   <implemented> + <unimplemented> + <implemented>
      // If we delegate `<unimplemented>` part to the legacy parser, it would
      // eagerly parse the rest of the expression, instead of just the
      // <unimplemented> part.
      // Maybe call 'Parser::parseExprSequenceElement' directly.
      break
    case // Known unimplemented kinds.
        .arrowExpr, .asExpr, .assignmentExpr, .awaitExpr, .binaryOperatorExpr,
        .borrowExpr, .canImportExpr, .canImportVersionInfo, .dictionaryExpr,
        .discardAssignmentExpr, .doExpr, .editorPlaceholderExpr,
        .floatLiteralExpr, .forceUnwrapExpr, .inOutExpr, .infixOperatorExpr,
        .isExpr, .keyPathExpr, .macroExpansionExpr, .consumeExpr, .copyExpr,
        .optionalChainingExpr, .packElementExpr, .packExpansionExpr,
        .postfixIfConfigExpr, .postfixOperatorExpr, .prefixOperatorExpr,
        .regexLiteralExpr, .sequenceExpr, .genericSpecializationExpr,
        .simpleStringLiteralExpr, .subscriptCallExpr, .superExpr, .switchExpr,
        .ternaryExpr, .tryExpr, .typeExpr, .unresolvedAsExpr, .unresolvedIsExpr,
        .patternExpr, .unresolvedTernaryExpr:
      return false
    case // Unknown expr kinds.
      _ where current.is(ExprSyntax.self):
      return false
    default:
      break
    }
    if current.id == node.id {
      return true
    }
    // This is walking up the parents from the first token of `node`. `.parent`
    // must exist if `current` is not `node`
    current = current.parent!
  }
}

extension ASTGenVisitor {
  public func generate(_ node: ClosureExprSyntax) -> BridgedClosureExpr {
    let body = BridgedBraceStmt.createParsed(
      self.ctx,
      lBraceLoc: node.leftBrace.bridgedSourceLoc(in: self),
      elements: self.generate(node.statements),
      rBraceLoc: node.rightBrace.bridgedSourceLoc(in: self)
    )

    // FIXME: Translate the signature, capture list, 'in' location, etc.
    return .createParsed(self.ctx, declContext: self.declContext, body: body)
  }

  public func generate(_ node: FunctionCallExprSyntax) -> BridgedCallExpr {
    if !node.arguments.isEmpty || node.trailingClosure == nil {
      if node.leftParen == nil {
        self.diagnose(
          Diagnostic(node: node, message: MissingChildTokenError(parent: node, kindOfTokenMissing: .leftParen))
        )
      }
      if node.rightParen == nil {
        self.diagnose(
          Diagnostic(node: node, message: MissingChildTokenError(parent: node, kindOfTokenMissing: .rightParen))
        )
      }
    }

    var node = node

    // Transform the trailing closure into an argument.
    if let trailingClosure = node.trailingClosure {
      let tupleElement = LabeledExprSyntax(
        label: nil,
        colon: nil,
        expression: ExprSyntax(trailingClosure),
        trailingComma: nil
      )

      node.arguments.append(tupleElement)
      node.trailingClosure = nil
    }

    let argumentTuple = self.generate(
      node.arguments,
      leftParen: node.leftParen,
      rightParen: node.rightParen
    )
    let callee = generate(node.calledExpression)

    return .createParsed(self.ctx, fn: callee, args: argumentTuple)
  }

  public func generate(_ node: DeclReferenceExprSyntax) -> BridgedUnresolvedDeclRefExpr {
    let (name, nameLoc) = node.baseName.bridgedIdentifierAndSourceLoc(in: self)

    return .createParsed(self.ctx, base: name, loc: nameLoc)
  }

  public func generate(_ node: IdentifierPatternSyntax) -> BridgedUnresolvedDeclRefExpr {
    let (name, nameLoc) = node.identifier.bridgedIdentifierAndSourceLoc(in: self)

    return .createParsed(self.ctx, base: name, loc: nameLoc)
  }

  public func generate(_ node: MemberAccessExprSyntax) -> BridgedUnresolvedDotExpr {
    let loc = node.bridgedSourceLoc(in: self)
    let base = generate(node.base!)
    let name = node.declName.baseName.bridgedIdentifier(in: self)

    return .createParsed(ctx, base: base, dotLoc: loc, name: name, nameLoc: loc)
  }

  public func generate(_ node: IfExprSyntax) -> BridgedSingleValueStmtExpr {
    let stmt = makeIfStmt(node).asStmt

    // Wrap in a SingleValueStmtExpr to embed as an expression.
    return .createWithWrappedBranches(
      ctx,
      stmt: stmt,
      declContext: declContext,
      mustBeExpr: true
    )
  }

  public func generate(_ node: TupleExprSyntax) -> BridgedTupleExpr {
    return self.generate(node.elements, leftParen: node.leftParen, rightParen: node.rightParen)
  }

  // NOTE: When implementing new `generate(_:)`, please update `isExprMigrated(_:)`.
}

extension ASTGenVisitor {
  /// Generate a tuple expression from a ``LabeledExprListSyntax`` and parentheses.
  func generate(_ node: LabeledExprListSyntax, leftParen: TokenSyntax?, rightParen: TokenSyntax?) -> BridgedTupleExpr {
    let expressions = node.lazy.map {
      self.generate($0.expression)
    }
    let labels = node.lazy.map {
      $0.label.bridgedIdentifier(in: self)
    }
    let labelLocations = node.lazy.map {
      if let label = $0.label {
        return label.bridgedSourceLoc(in: self)
      }

      return $0.bridgedSourceLoc(in: self)
    }

    return BridgedTupleExpr.createParsed(
      self.ctx,
      leftParenLoc: leftParen.bridgedSourceLoc(in: self),
      exprs: expressions.bridgedArray(in: self),
      labels: labels.bridgedArray(in: self),
      labelLocs: labelLocations.bridgedArray(in: self),
      rightParenLoc: rightParen.bridgedSourceLoc(in: self)
    )
  }
}
