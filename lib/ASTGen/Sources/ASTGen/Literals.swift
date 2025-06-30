//===--- Literals.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
@_spi(Compiler) import SwiftParser
@_spi(RawSyntax) import SwiftSyntax
@_spi(CompilerInterface) import _CompilerRegexParser

extension ASTGenVisitor {
  func generate(stringLiteralExpr node: StringLiteralExprSyntax) -> BridgedExpr {
    if node.segments.allSatisfy({ $0.is(StringSegmentSyntax.self) }) {
      return self.generateStringLiteralExpr(stringLiteralExpr: node).asExpr
    } else {
      return self.generateInterpolatedStringLiteralExpr(stringLiteralExpr: node).asExpr
    }
  }

  func generateStringLiteralExpr(stringLiteralExpr node: StringLiteralExprSyntax) -> BridgedStringLiteralExpr {
    // This generator only handles non-interpolated string literal.
    assert(node.segments.allSatisfy({ $0.is(StringSegmentSyntax.self) }))

    let stringLiteralKind = node.stringLiteralKind ?? .singleLine
    let delimiterLength = node.delimiterLength

    var value: String = ""
    for case .stringSegment(let seg) in node.segments where !seg.hasError {
      seg.appendUnescapedLiteralValue(
        stringLiteralKind: stringLiteralKind,
        delimiterLength: delimiterLength,
        to: &value
      )
    }
    return value.withBridgedString {
      BridgedStringLiteralExpr.createParsed(
        self.ctx,
        value: $0,
        loc: self.generateSourceLoc(node)
      )
    }
  }

  func generateInterpolatedStringLiteralExpr(stringLiteralExpr node: StringLiteralExprSyntax) -> BridgedInterpolatedStringLiteralExpr {
    // Non-interpolated string literal should use 'generateStringLiteralExpr()' above.
    assert(node.segments.contains(where: { $0.is(ExpressionSegmentSyntax.self) }))

    let stringLiteralKind = node.stringLiteralKind ?? .singleLine
    let delimiterLength = node.delimiterLength
    let startLoc = self.generateSourceLoc(node)
    let afterQuoteLoc: BridgedSourceLoc = {
      var l = startLoc
      if let pound = node.openingPounds {
        l = l.advanced(by: pound.trimmedLength.utf8Length)
        l = l.advanced(by: pound.trailingTriviaLength.utf8Length)
        l = l.advanced(by: node.openingQuote.leadingTriviaLength.utf8Length)
      }
      l = l.advanced(by: node.openingQuote.trimmedLength.utf8Length)
      return l
    }()

    // 'stmts' is a list of body elements of 'TapExpr' aka "appendingExpr" for the 'InterpolatedStringLiteralExpr'.
    var stmts: [BridgedASTNode] = []

    // The first element is a 'VarDecl'.
    let interpolationVar = BridgedVarDecl.createImplicitStringInterpolationVar(self.declContext)
    stmts.append(.decl(interpolationVar.asDecl))

    // Name reference to `appendLiteral(_:)`
    let appendLiteral = BridgedDeclNameRef.createParsed(
      self.ctx,
      baseName: .init(self.ctx.getIdentifier("appendLiteral")),
      argumentLabels: CollectionOfOne(Identifier()).bridgedArray(in: self)
    )
    // Name reference to `appendInterpolation`. Arguments labels are not determined yet.
    let appendInterpolation = BridgedDeclNameRef.createParsed(
      .init(self.ctx.getIdentifier("appendInterpolation"))
    )

    // Total byte length of "literal" segments.
    var literalCapacity: Int = 0
    // Count of "expression" segments.
    var interpolationCount = 0

    // In multi-line string literals, each line has '.stringSegment' even without
    // interpolations. We need to join them into single string literal value in AST.
    var currLiteral: (value: String, loc: BridgedSourceLoc)? = nil
    var isFirst = true
    func consumeCurrentLiteralValue() {
      guard var literal = currLiteral else {
        return
      }
      currLiteral = nil

      // Construct '$interpolation.appendLiteral(_:)(literalValue)'
      let literalExpr: BridgedStringLiteralExpr = literal.value.withBridgedString { bridgedValue in
        literalCapacity += bridgedValue.count
        return .createParsed(
          self.ctx,
          value: bridgedValue,
          loc: isFirst ? startLoc : literal.loc
        )
      }
      let interpolationVarRef = BridgedDeclRefExpr.create(
        self.ctx,
        decl: interpolationVar.asDecl,
        loc: BridgedDeclNameLoc.createParsed(isFirst ? afterQuoteLoc : literal.loc),
        isImplicit: true
      )
      let appendLiteralRef = BridgedUnresolvedDotExpr.createParsed(
        self.ctx,
        base: interpolationVarRef.asExpr,
        dotLoc: nil,
        name: appendLiteral,
        nameLoc: BridgedDeclNameLoc()
      )
      appendLiteralRef.asExpr.setImplicit()
      let argList = BridgedArgumentList.createImplicitUnlabeled(
        self.ctx,
        exprs: CollectionOfOne(literalExpr).bridgedArray(in: self)
      )
      let callExpr = BridgedCallExpr.createParsed(
        self.ctx,
        fn: appendLiteralRef.asExpr,
        args: argList
      )
      callExpr.asExpr.setImplicit()
      stmts.append(.expr(callExpr.asExpr))

      isFirst = false
    }

    for seg in node.segments {
      switch seg {
      case .stringSegment(let seg):
        if currLiteral == nil {
          currLiteral = (
            value: "",
            loc: self.generateSourceLoc(seg)
          )
        }
        if seg.hasError {
          continue
        }
        seg.appendUnescapedLiteralValue(
          stringLiteralKind: stringLiteralKind,
          delimiterLength: delimiterLength,
          to: &currLiteral!.value
        )
      case .expressionSegment(let seg):
        // Consume literals before this interpolation.
        consumeCurrentLiteralValue()

        let loc = self.generateSourceLoc(seg)

        // Construct '$interpolation.appendExpression(<interpolation value>)'
        let interpolationVarRef = BridgedDeclRefExpr.create(
          self.ctx,
          decl: interpolationVar.asDecl,
          loc: BridgedDeclNameLoc.createParsed(loc),
          isImplicit: true
        )
        let appendInterpolationRef = BridgedUnresolvedDotExpr.createParsed(
          self.ctx,
          base: interpolationVarRef.asExpr,
          dotLoc: self.generateSourceLoc(seg.backslash),
          name: appendInterpolation,
          nameLoc: BridgedDeclNameLoc.createParsed(self.generateSourceLoc(seg))
        )
        appendInterpolationRef.asExpr.setImplicit()
        let argList = self.generateArgumentList(
          leftParen: seg.leftParen,
          labeledExprList: seg.expressions,
          rightParen: seg.rightParen,
          trailingClosure: nil,
          additionalTrailingClosures: nil
        )
        let callExpr = BridgedCallExpr.createParsed(
          self.ctx,
          fn: appendInterpolationRef.asExpr,
          args: argList
        )
        stmts.append(.expr(callExpr.asExpr))

        interpolationCount += 1
      }
    }

    // Consume remaining literal value.
    consumeCurrentLiteralValue()

    let body = BridgedBraceStmt.createParsed(
      self.ctx,
      lBraceLoc: nil,
      elements: stmts.lazy.bridgedArray(in: self),
      rBraceLoc: nil
    )
    let appendingExpr = BridgedTapExpr.create(
      self.ctx,
      body: body
    )
    return BridgedInterpolatedStringLiteralExpr.createParsed(
      self.ctx,
      loc: startLoc,
      literalCapacity: literalCapacity,
      interpolationCount: interpolationCount,
      appendingExpr: appendingExpr
    )
  }

  func copyAndStripUnderscores(text: SyntaxText) -> BridgedStringRef {
    assert(!text.isEmpty)
    let start = self.ctx.allocate(size: text.count, alignment: 1)!
      .bindMemory(to: UInt8.self, capacity: text.count)
    var ptr = start
    for chr in text where chr != UInt8(ascii: "_") {
      ptr.initialize(to: chr)
      ptr += 1
    }
    return BridgedStringRef(
      data: start,
      count: start.distance(to: ptr)
    )
  }

  func generate(integerLiteralExpr node: IntegerLiteralExprSyntax) -> BridgedIntegerLiteralExpr {
    return .createParsed(
      self.ctx,
      value: self.copyAndStripUnderscores(text: node.literal.rawText),
      loc: self.generateSourceLoc(node)
    )
  }

  func generate(floatLiteralExpr node: FloatLiteralExprSyntax) -> BridgedFloatLiteralExpr {
    return .createParsed(
      self.ctx,
      value: self.copyAndStripUnderscores(text: node.literal.rawText),
      loc: self.generateSourceLoc(node)
    )
  }

  func generate(booleanLiteralExpr node: BooleanLiteralExprSyntax) -> BridgedBooleanLiteralExpr {
    let value = node.literal.keywordKind == .true
    return .createParsed(
      ctx,
      value: value,
      loc: self.generateSourceLoc(node.literal)
    )
  }

  func generate(arrayExpr node: ArrayExprSyntax) -> BridgedArrayExpr {
    let expressions = node.elements.lazy.map({ self.generate(expr: $0.expression) })

    let commaLocations = node.elements.compactMap(in: self) {
      self.generateSourceLoc($0.trailingComma)
    }

    return .createParsed(
      self.ctx,
      lSquareLoc: self.generateSourceLoc(node.leftSquare),
      elements: expressions.bridgedArray(in: self),
      commaLocs: commaLocations,
      rSquareLoc: self.generateSourceLoc(node.rightSquare)
    )
  }

  private func generate(dictionaryElement node: DictionaryElementSyntax) -> BridgedTupleExpr {
    return BridgedTupleExpr.createParsedDictionaryElement(
      self.ctx,
      key: self.generate(expr: node.key),
      value: self.generate(expr: node.value)
    )
  }

  public func generate(dictionaryExpr node: DictionaryExprSyntax) -> BridgedDictionaryExpr {
    let lBracketLoc = self.generateSourceLoc(node.leftSquare)
    let rBracketLoc = self.generateSourceLoc(node.rightSquare)
    let elements: BridgedArrayRef
    let colonLocs: BridgedArrayRef

    switch node.content {
    case .colon(_):
      elements = BridgedArrayRef()
      colonLocs = BridgedArrayRef()
    case .elements(let elementNodes):
      elements = elementNodes.lazy
        .map({ self.generate(dictionaryElement: $0).asExpr })
        .bridgedArray(in: self)
      colonLocs = elementNodes.lazy
        .map({ self.generateSourceLoc($0.colon) })
        .bridgedArray(in: self)
    }
    return .createParsed(
      self.ctx,
      lBracketLoc: lBracketLoc,
      elements: elements,
      colonLocs: colonLocs,
      rBracketLoc: rBracketLoc
    )
  }

  func generate(nilLiteralExpr node: NilLiteralExprSyntax) -> BridgedNilLiteralExpr {
    .createParsed(
      self.ctx,
      nilKeywordLoc: self.generateSourceLoc(node.nilKeyword)
    )
  }
}
