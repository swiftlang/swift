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

      // `generate(stringLiteralExpr:)` doesn't support interpolations.
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
  func generate(expr node: ExprSyntax) -> BridgedExpr {
    guard isExprMigrated(node) else {
      return generateWithLegacy(node)
    }
    switch node.as(ExprSyntaxEnum.self) {
    case .arrayExpr(let node):
      return self.generate(arrayExpr: node).asExpr
    case .arrowExpr:
      break
    case .asExpr:
      break
    case .assignmentExpr:
      break
    case .awaitExpr:
      break
    case .binaryOperatorExpr:
      break
    case .booleanLiteralExpr(let node):
      return self.generate(booleanLiteralExpr: node).asExpr
    case .borrowExpr:
      break
    case .canImportExpr:
      break
    case .canImportVersionInfo:
      break
    case .closureExpr(let node):
      return self.generate(closureExpr: node).asExpr
    case .consumeExpr:
      break
    case .copyExpr:
      break
    case .declReferenceExpr(let node):
      return self.generate(declReferenceExpr: node).asExpr
    case .dictionaryExpr:
      break
    case .discardAssignmentExpr:
      break
    case .doExpr:
      break
    case .editorPlaceholderExpr:
      break
    case .floatLiteralExpr:
      break
    case .forceUnwrapExpr:
      break
    case .functionCallExpr(let node):
      return self.generate(functionCallExpr: node).asExpr
    case .genericSpecializationExpr:
      break
    case .ifExpr(let node):
      return self.generate(ifExpr: node).asExpr
    case .inOutExpr:
      break
    case .infixOperatorExpr:
      break
    case .integerLiteralExpr(let node):
      return self.generate(integerLiteralExpr: node).asExpr
    case .isExpr:
      break
    case .keyPathExpr:
      break
    case .macroExpansionExpr:
      break
    case .memberAccessExpr(let node):
      return self.generate(memberAccessExpr: node).asExpr
    case .missingExpr:
      break
    case .nilLiteralExpr(let node):
      return self.generate(nilLiteralExpr: node).asExpr
    case .optionalChainingExpr:
      break
    case .packElementExpr:
      break
    case .packExpansionExpr:
      break
    case .patternExpr:
      break
    case .postfixIfConfigExpr:
      break
    case .postfixOperatorExpr:
      break
    case .prefixOperatorExpr:
      break
    case .regexLiteralExpr:
      break
    case .sequenceExpr:
      break
    case .simpleStringLiteralExpr:
      break
    case .stringLiteralExpr(let node):
      return self.generate(stringLiteralExpr: node).asExpr
    case .subscriptCallExpr:
      break
    case .superExpr:
      break
    case .switchExpr:
      break
    case .ternaryExpr:
      break
    case .tryExpr:
      break
    case .tupleExpr(let node):
      return self.generate(tupleExpr: node).asExpr
    case .typeExpr:
      break
    case .unresolvedAsExpr:
      break
    case .unresolvedIsExpr:
      break
    case .unresolvedTernaryExpr:
      break
    }
    preconditionFailure("isExprMigrated() mismatch")
  }

  public func generate(closureExpr node: ClosureExprSyntax) -> BridgedClosureExpr {
    let body = BridgedBraceStmt.createParsed(
      self.ctx,
      lBraceLoc: node.leftBrace.bridgedSourceLoc(in: self),
      elements: self.generate(codeBlockItemList: node.statements),
      rBraceLoc: node.rightBrace.bridgedSourceLoc(in: self)
    )

    // FIXME: Translate the signature, capture list, 'in' location, etc.
    return .createParsed(self.ctx, declContext: self.declContext, body: body)
  }

  public func generate(functionCallExpr node: FunctionCallExprSyntax) -> BridgedCallExpr {
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
      labeledExprList: node.arguments,
      leftParen: node.leftParen,
      rightParen: node.rightParen
    )
    let callee = generate(expr: node.calledExpression)

    return .createParsed(self.ctx, fn: callee, args: argumentTuple)
  }

  public func generate(declReferenceExpr node: DeclReferenceExprSyntax) -> BridgedUnresolvedDeclRefExpr {
    let (name, nameLoc) = node.baseName.bridgedIdentifierAndSourceLoc(in: self)

    return .createParsed(self.ctx, base: name, loc: nameLoc)
  }

  public func generate(memberAccessExpr node: MemberAccessExprSyntax) -> BridgedUnresolvedDotExpr {
    let loc = node.bridgedSourceLoc(in: self)
    let base = generate(expr: node.base!)
    let name = node.declName.baseName.bridgedIdentifier(in: self)

    return .createParsed(ctx, base: base, dotLoc: loc, name: name, nameLoc: loc)
  }

  public func generate(ifExpr node: IfExprSyntax) -> BridgedSingleValueStmtExpr {
    let stmt = makeIfStmt(node).asStmt

    // Wrap in a SingleValueStmtExpr to embed as an expression.
    return .createWithWrappedBranches(
      ctx,
      stmt: stmt,
      declContext: declContext,
      mustBeExpr: true
    )
  }

  public func generate(tupleExpr node: TupleExprSyntax) -> BridgedTupleExpr {
    return self.generate(labeledExprList: node.elements, leftParen: node.leftParen, rightParen: node.rightParen)
  }

  // NOTE: When implementing new `generate(expr:)`, please update `isExprMigrated(_:)`.
}

extension ASTGenVisitor {
  /// Generate a tuple expression from a ``LabeledExprListSyntax`` and parentheses.
  func generate(labeledExprList node: LabeledExprListSyntax, leftParen: TokenSyntax?, rightParen: TokenSyntax?) -> BridgedTupleExpr {
    let expressions = node.lazy.map {
      self.generate(expr: $0.expression)
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
