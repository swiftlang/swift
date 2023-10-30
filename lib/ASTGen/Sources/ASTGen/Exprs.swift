import ASTBridging
import SwiftDiagnostics
import SwiftSyntax

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
    self.generate(node.elements, leftParen: node.leftParen, rightParen: node.rightParen)
  }
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
