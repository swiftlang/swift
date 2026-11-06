//===--- BuiltinPound.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
@_spi(RawSyntax) import SwiftSyntax

extension ASTGenVisitor {
  /// 3 state result:
  ///  * successfully generated: `.generated(.some(T))`
  ///  * handled but failed: `.generated(.none)`
  ///  * fallback to default behavior: `.ignored`
  enum MaybeGenerated<T> {
    case generated(T?)
    case ignored
  }

  func maybeGenerateBuiltinPound(macroExpansionDecl node: MacroExpansionDeclSyntax) -> MaybeGenerated<BridgedASTNode> {
    let result = maybeGenerateBuiltinPound(freestandingMacroExpansion: node)

    switch result {
    case .generated(_):
      // A built-in pound is not a declaration, so it cannot carry attributes or
      // modifiers. The C++ parser fails to parse a declaration at the pound and
      // reports that, but still evaluates the directive, so keep the generated
      // node rather than dropping it.
      if !node.attributes.isEmpty || !node.modifiers.isEmpty {
        self.diagnose(.expected_decl, at: node.pound)
      }
      break
    case .ignored:
      break
    }

    return result
  }

  /// Handle built in pound keywords.
  func maybeGenerateBuiltinPound(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax) -> MaybeGenerated<BridgedASTNode> {
    // '#Module::name' is always a macro expansion, even if 'name' matches a
    // builtin pound keyword (e.g., '#Testing::sourceLocation').
    if node.moduleSelector != nil {
      return .ignored
    }

    self.diagnoseLegacyLineDirective(node)

    let macroNameText = node.macroName.rawText;

    // '#file', '#line' etc.
    let magicIdentifierKind = BridgedMagicIdentifierLiteralKind(from: macroNameText.bridged)
    if magicIdentifierKind != .none {
      let magicIdentifierExpr = self.generateMagicIdentifierExpr(
        freestandingMacroExpansion: node,
        kind: magicIdentifierKind
      )
      return .generated(.expr(magicIdentifierExpr.asExpr))
    }

    // '#colorLiteral' et al.
    let objectLiteralKind = BridgedObjectLiteralKind(from: macroNameText.bridged)
    if objectLiteralKind != .none {
      let objectLiteralExpr = self.generateObjectLiteralExpr(
        freestandingMacroExpansion: node,
        kind: objectLiteralKind
      )
      return .generated(.expr(objectLiteralExpr.asExpr))
    }

    // Other built-in pound syntax.
    let keyword = BridgedPoundKeyword(from: macroNameText.bridged)
    switch keyword {
    case .selector:
      let selectorExpr = self.generateObjCSelectorExpr(freestandingMacroExpansion: node)
      return .generated(.expr(selectorExpr))

    case .keyPath:
      let keypathExpr = self.generateObjCKeyPathExpr(freestandingMacroExpansion: node)
      return .generated(.expr(keypathExpr))

    case .assert where ctx.langOpts.hasFeature(.StaticAssert):
      let assertStmtOpt = self.generatePoundAssertStmt(freestandingMacroExpansion: node)
      return .generated(assertStmtOpt.map({ .stmt($0.asStmt) }))

    case .error, .warning:
      self.handlePoundDiagnostic(freestandingMacroExpansion: node, kind: keyword == .error ? .error : .warning)
      // '#error' and '#warning' don't produce an AST node.
      return .generated(nil)

    case ._hasSymbol, .available, .unavailable:
      // These are conditions, not expressions. The C++ parser
      // (`Parser::parseExprPrimary`) parses them here anyway for better
      // recovery, rejects them, and yields an ErrorExpr; do the same.
      self.diagnose(
        .special_condition_outside_if_stmt_guard(
          "#" + String(syntaxText: node.macroName.rawText)
        ),
        at: node.macroName
      )
      return .generated(
        .expr(
          BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr
        )
      )
      // return .generated(nil)

    case .none, .assert:
      // Not a builtin pound keyword.
      return .ignored

    case .file, .fileID, .filePath, .function, .line, .column, .dsohandle,
        .colorLiteral, .imageLiteral, .fileLiteral:
      // Should be handled above
      fatalError("(compiler bug) unreachable")

    case .if, .elseif, .else, .endif, .sourceLocation:
      fatalError("(compiler bug) builtin pound keyword as macro expansion expr")
    }
  }

  enum PoundDiagnosticKind {
    case warning, error
  }

  func handlePoundDiagnostic(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax, kind: PoundDiagnosticKind) {
    let isError = kind == .error

    switch node.parent?.kind {
    case .codeBlockItem, .memberBlockItem, .switchCaseList, nil:
      break
    default:
      // Not at declaration level. The parser rejects this placement, so there is
      // nothing to add here.
      return
    }

    guard
      node.genericArgumentClause == nil,
      node.trailingClosure == nil,
      node.additionalTrailingClosures.isEmpty
    else {
      // Recovery output; the parser has already diagnosed it.
      return
    }

    // The C++ parser rejects attributes and modifiers, but still diagnoses the
    // directive itself. swift-syntax diagnoses them already in a member list,
    // but not among statements and top-level declarations.
    if let decl = node.as(MacroExpansionDeclSyntax.self), node.parent?.kind != .memberBlockItem,
      !decl.attributes.isEmpty || !decl.modifiers.isEmpty
    {
      self.diagnose(.expected_decl, at: decl.pound)
    }

    // Check the shape of the directive the way `Parser::parseDeclPoundDiagnostic`
    // does. swift-syntax parses `#error` as a macro expansion, so the malformed
    // forms the C++ parser rejects -- no parentheses, an unbalanced parenthesis,
    // text that is not a string literal, trailing tokens -- come out as
    // ordinary-looking expansions or as unrelated statements, and the tokens
    // have to be examined directly.
    func atStartOfLine(_ token: TokenSyntax) -> Bool {
      token.leadingTrivia.contains { $0.isNewline }
    }
    func insertion(_ text: String, at position: AbsolutePosition) -> CompilerDiagnosticFixIt {
      .init(replace: self.generateCharSourceRange(start: position, length: SourceLength(utf8Length: 0)), with: text)
    }

    let nameToken = node.macroName
    guard var token = nameToken.nextToken(viewMode: .sourceAccurate), token.rawTokenKind != .endOfFile,
      !atStartOfLine(token)
    else {
      // E.g. `#warning` alone on its line.
      let next = nameToken.nextToken(viewMode: .sourceAccurate) ?? nameToken
      self.diagnose(
        .pound_diagnostic_expected_parens(isError),
        at: next,
        fixIts: [insertion("(\"<#message#>\")", at: nameToken.endPositionBeforeTrailingTrivia)]
      )
      return
    }

    let lParen = token
    var previous = nameToken
    let hadLParen = token.rawTokenKind == .leftParen
    if hadLParen {
      previous = token
      guard let next = token.nextToken(viewMode: .sourceAccurate) else { return }
      token = next
    }

    // Anything but a string literal: `#warning(oops, forgot the quotes)`.
    func isStringLiteralStart(_ token: TokenSyntax) -> Bool {
      switch token.rawTokenKind {
      case .stringQuote, .multilineStringQuote, .rawStringPoundDelimiter: return true
      default: return false
      }
    }
    guard isStringLiteralStart(token) else {
      let wordsStart = token
      var stopper = token
      var lastConsumed = previous
      while stopper.rawTokenKind != .endOfFile, stopper.rawTokenKind != .rightParen, !atStartOfLine(stopper) {
        lastConsumed = stopper
        guard let next = stopper.nextToken(viewMode: .sourceAccurate) else { break }
        stopper = next
      }
      let wordsStartPosition = wordsStart.positionAfterSkippingLeadingTrivia
      let wordsEndPosition = lastConsumed.endPositionBeforeTrailingTrivia
      var fixIts: [CompilerDiagnosticFixIt] = []
      if wordsEndPosition != wordsStartPosition {
        fixIts.append(insertion(hadLParen ? "\"" : "(\"", at: wordsStartPosition))
        fixIts.append(insertion(stopper.rawTokenKind == .rightParen ? "\"" : "\")", at: wordsEndPosition))
      }
      self.diagnose(
        .pound_diagnostic_expected_string(isError),
        at: SourceLoc(at: wordsStartPosition, in: self.base),
        fixIts: fixIts
      )
      return
    }

    var literalAncestor: Syntax? = Syntax(token)
    while let candidate = literalAncestor, !candidate.is(StringLiteralExprSyntax.self) {
      literalAncestor = candidate.parent
    }
    guard let literal = literalAncestor?.as(StringLiteralExprSyntax.self),
      let literalLast = literal.lastToken(viewMode: .sourceAccurate)
    else {
      return
    }

    var after = literalLast.nextToken(viewMode: .sourceAccurate)
    var hadRParen = false
    if let rParen = after, rParen.rawTokenKind == .rightParen {
      hadRParen = true
      after = rParen.nextToken(viewMode: .sourceAccurate)
    }

    if let after, after.rawTokenKind != .endOfFile, !atStartOfLine(after) {
      self.diagnose(.extra_tokens_pound_diagnostic_directive(isError), at: after)
      return
    }

    if !hadLParen && !hadRParen {
      // E.g. `#warning "foo"`.
      self.diagnose(
        .pound_diagnostic_expected_parens(isError),
        at: lParen,
        highlight: Syntax(literal),
        fixIts: [
          insertion("(", at: literal.positionAfterSkippingLeadingTrivia),
          insertion(")", at: literal.endPositionBeforeTrailingTrivia),
        ]
      )
      return
    } else if hadRParen && !hadLParen {
      // E.g. `#warning "foo")`.
      self.diagnose(
        .pound_diagnostic_expected("(", isError),
        at: literal,
        fixIts: [insertion("(", at: literal.positionAfterSkippingLeadingTrivia)]
      )
      return
    } else if hadLParen && !hadRParen {
      // E.g. `#warning("foo"`.
      self.diagnose(
        .pound_diagnostic_expected(")", isError),
        at: literalLast,
        fixIts: [insertion(")", at: literal.endPositionBeforeTrailingTrivia)]
      )
      return
    }

    guard literal.segments.allSatisfy({ $0.is(StringSegmentSyntax.self) }) else {
      self.diagnose(.pound_diagnostic_interpolation(isError), at: literal, highlight: Syntax(literal))
      return
    }

    guard let message = literal.representedLiteralValue else {
      // A malformed literal, e.g. an unterminated multi-line string. The lexer
      // has already reported it; there is no message to relay.
      return
    }

    // Unconditionally emit the diagnostic. Inactive #if regions are not generated.
    self.diagnose(.poundDiagnostic(literal, message: message, isError: isError))
  }

  func generatePoundAssertStmt(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax) -> BridgedPoundAssertStmt? {
    assert(self.ctx.langOpts.hasFeature(.StaticAssert))
    var args = node.arguments[...]
    let conditionExpr = self.generateConsumingAttrOption(args: &args, label: nil) { conditionNode in
      self.generate(expr: conditionNode)
    }
    guard let conditionExpr else {
      return nil
    }
    let message: BridgedStringRef?
    if !args.isEmpty {
      message = self.generateConsumingSimpleStringLiteralAttrOption(args: &args)
    } else {
      message = nil
    }

    return .createParsed(
      self.ctx,
      range: self.generateSourceRange(node),
      condition: conditionExpr,
      message: message ?? BridgedStringRef()
    )
  }

  func generateMagicIdentifierExpr(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax, kind: BridgedMagicIdentifierLiteralKind) -> BridgedMagicIdentifierLiteralExpr {
    guard node.lastToken(viewMode: .sourceAccurate) == node.macroName else {
      // TODO: Diagnose.
      fatalError("magic identifier token with arguments")
    }

    // `#file` is spelled the same for two kinds, and the name lookup finds the
    // first. It means `#fileID` only with ConciseMagicFile, and `#filePath`
    // otherwise.
    var kind = kind
    if kind == .fileIDSpelledAsFile, !self.ctx.langOpts.hasFeature(.ConciseMagicFile) {
      kind = .filePathSpelledAsFile
    }

    return BridgedMagicIdentifierLiteralExpr.createParsed(
      self.ctx,
      kind: kind,
      loc: self.generateSourceLoc(node.pound)
    )
  }

  func generateObjectLiteralExpr(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax, kind: BridgedObjectLiteralKind) -> BridgedObjectLiteralExpr {
    guard
      node.genericArgumentClause == nil,
      node.trailingClosure == nil,
      node.additionalTrailingClosures.isEmpty
    else {
      // TODO: Diagnose.
      fatalError("object identifier with generic specialization")
    }

    return BridgedObjectLiteralExpr.createParsed(
      self.ctx,
      poundLoc: self.generateSourceLoc(node.pound),
      kind: kind,
      args: self.generateArgumentList(
        leftParen: node.leftParen,
        labeledExprList: node.arguments,
        rightParen: node.rightParen,
        trailingClosure: nil,
        additionalTrailingClosures: nil
      )
    )
  }

  /// The location of `token`, or of `node`'s pound keyword when the parser
  /// synthesized `token` during recovery.
  ///
  /// A synthesized token still has a position -- a zero-width one past the
  /// preceding token and its trailing trivia -- so diagnosing at it can land
  /// well away from the construct it belongs to.
  func generateSourceLoc(
    _ token: TokenSyntax?,
    orPoundOf node: some FreestandingMacroExpansionSyntax
  ) -> SourceLoc {
    let loc = self.generateSourceLocIfPresent(token)
    if loc == nil {
      return self.generateSourceLoc(node.pound)
    }
    return loc
  }

  /// Where to report that `#selector`/`#keyPath` was written with something the
  /// grammar does not admit after the keyword -- a generic argument clause or a
  /// trailing closure.
  ///
  /// The C++ parser has no production for either, so it stops right after the
  /// keyword and reports a missing '(' there. Point at whichever of the two
  /// appeared, matching that column.
  func poundArgumentListMismatchLoc(
    _ node: some FreestandingMacroExpansionSyntax
  ) -> SourceLoc {
    if let genericArgumentClause = node.genericArgumentClause {
      return self.generateSourceLoc(genericArgumentClause)
    }
    if let trailingClosure = node.trailingClosure {
      return self.generateSourceLoc(trailingClosure)
    }
    return self.generateSourceLoc(node.additionalTrailingClosures)
  }

  func generateObjCSelectorExpr(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax) -> BridgedExpr {
    guard
      node.genericArgumentClause == nil,
      node.trailingClosure == nil,
      node.additionalTrailingClosures.isEmpty
    else {
      // `#selector` takes neither generic arguments nor a trailing closure. The
      // C++ parser has no grammar for either, so it fails looking for the '('
      // that should have followed the keyword.
      self.diagnose(
        .expr_selector_expected_lparen,
        at: self.poundArgumentListMismatchLoc(node)
      )
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr
    }

    var args = node.arguments[...]
    guard let arg = args.popFirst() else {
      // `#selector()`. The C++ parser reports the missing method expression and
      // recovers with an ErrorExpr; nothing here needs the argument. It points
      // at the ')', so fall back to the pound if recovery synthesized one.
      self.diagnose(
        .expr_selector_expected_method_expr,
        at: self.generateSourceLoc(node.rightParen, orPoundOf: node)
      )
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr
    }
    let kind: BridgedObjCSelectorKind
    switch arg.label?.rawText {
    case nil: kind = .method
    case "getter": kind = .getter
    case "setter": kind = .setter
    case _?:
      // Only 'getter:' and 'setter:' are labels here. The C++ parser has no
      // grammar for any other label: it parses the label as the method
      // expression and then fails looking for the ')' at the colon.
      self.diagnose(.expr_selector_expected_rparen, at: self.generateSourceLoc(arg.colon, orPoundOf: node))
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr
    }
    let expr = self.generate(expr: arg.expression)
    guard args.isEmpty else {
      // `#selector` takes exactly one argument; the C++ parser stops after it
      // and fails looking for the ')' at the comma.
      self.diagnose(.expr_selector_expected_rparen, at: self.generateSourceLoc(arg.trailingComma, orPoundOf: node))
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr
    }
    return BridgedObjCSelectorExpr.createParsed(
      self.ctx,
      kind: kind,
      keywordLoc: self.generateSourceLoc(node.pound),
      lParenLoc: self.generateSourceLoc(node.leftParen),
      modifierLoc: self.generateSourceLoc(arg.label),
      subExpr: expr,
      rParenLoc: self.generateSourceLoc(node.rightParen)
    ).asExpr
  }

  func generateObjCKeyPathExpr(freestandingMacroExpansion node: some FreestandingMacroExpansionSyntax) -> BridgedExpr {
    guard
      node.genericArgumentClause == nil,
      node.trailingClosure == nil,
      node.additionalTrailingClosures.isEmpty
    else {
      // As with `#selector`, neither form has a grammar, so the C++ parser fails
      // looking for the '(' that should have followed the keyword.
      self.diagnose(
        .expr_keypath_expected_lparen,
        at: self.poundArgumentListMismatchLoc(node)
      )
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr
    }

    var names: [BridgedDeclNameRef] = []
    var nameLocs: [BridgedDeclNameLoc] = []

    /// Collect the dotted chain of names in `node`, returning true if it is not
    /// one -- the only shape `#keyPath` accepts.
    func collectNames(expr node: ExprSyntax) -> Bool {
      if let declRefExpr = node.as(DeclReferenceExprSyntax.self) {
        let nameAndLoc = self.generateDeclNameRef(declReferenceExpr: declRefExpr)
        names.append(nameAndLoc.name)
        nameLocs.append(nameAndLoc.loc)
        return false
      }
      if let memberExpr = node.as(MemberAccessExprSyntax.self) {
        guard let base = memberExpr.base else {
          // An implicit member reference such as `#keyPath(.p)`: there is no
          // base to walk into.
          self.diagnose(.expr_keypath_expected_property_or_type, at: memberExpr)
          return true
        }
        if collectNames(expr: base) {
          return true
        }
        let nameAndLoc = self.generateDeclNameRef(declReferenceExpr: memberExpr.declName)
        names.append(nameAndLoc.name)
        nameLocs.append(nameAndLoc.loc)
        return false
      }
      // Anything else -- an operator expression, a parenthesized one, a literal.
      // The C++ parser reports this at the start of the offending expression.
      self.diagnose(.expr_keypath_expected_property_or_type, at: node)
      return true
    }

    var args = node.arguments[...]
    guard let arg = args.popFirst() else {
      // `#keyPath()`. As with `#selector()`, the C++ parser reports the missing
      // name at the ')' and recovers with an ErrorExpr.
      self.diagnose(
        .expr_keypath_expected_property_or_type,
        at: self.generateSourceLoc(node.rightParen, orPoundOf: node)
      )
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr
    }
    guard arg.label == nil else {
      // `#keyPath` takes no labels; the C++ parser parses the label as the first
      // name and then fails looking for the ')' at the colon.
      self.diagnose(.expr_keypath_expected_rparen, at: self.generateSourceLoc(arg.colon, orPoundOf: node))
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr
    }
    if /*hadError=*/collectNames(expr: arg.expression) {
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr;
    }

    guard args.isEmpty else {
      // `#keyPath` takes exactly one argument; the C++ parser stops after it and
      // fails looking for the ')' at the comma.
      self.diagnose(.expr_keypath_expected_rparen, at: self.generateSourceLoc(arg.trailingComma, orPoundOf: node))
      return BridgedErrorExpr.create(self.ctx, loc: self.generateSourceRange(node)).asExpr
    }

    return BridgedKeyPathExpr.createParsedPoundKeyPath(
      self.ctx,
      poundLoc: self.generateSourceLoc(node.pound),
      lParenLoc: self.generateSourceLoc(node.leftParen),
      names: names.lazy.bridgedArray(in: self),
      nameLocs: nameLocs.lazy.bridgedArray(in: self),
      rParenLoc: self.generateSourceLoc(node.rightParen)
    ).asExpr
  }
}
