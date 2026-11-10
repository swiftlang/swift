//===--- Patterns.swift ---------------------------------------------------===//
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
import BasicBridging
import SwiftDiagnostics
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

extension ASTGenVisitor {
  func generate(pattern node: PatternSyntax, typeAnnotation annotationNode: TypeAnnotationSyntax?) -> BridgedPattern {
    let pat = self.generate(pattern: node)
    if let annotationNode {
      return BridgedTypedPattern.createParsed(
        self.ctx,
        pattern: pat,
        type: self.generate(type: annotationNode.type)
      ).asPattern
    } else {
      return pat
    }
  }

  func generate(pattern node: PatternSyntax) -> BridgedPattern {
    switch node.as(PatternSyntaxEnum.self) {
    case .expressionPattern(let node):
      return self.generate(expressionPattern: node).asPattern
    case .identifierPattern(let node):
      // A name the parser could not make out, e.g. the keyword in `let self`,
      // has been diagnosed; binding it would only add noise about an unused
      // variable with no name.
      if node.identifier.presence == .missing {
        return BridgedAnyPattern.createParsed(
          self.ctx,
          loc: node.unexpectedBeforeIdentifier.map { self.generateSourceLoc($0) } ?? self.generateSourceLoc(node.identifier)
        ).asPattern
      }
      return self.generate(identifierPattern: node).asPattern
    case .isTypePattern(let node):
      return self.generate(isTypePattern: node).asPattern
    case .missingPattern(let node):
      return self.generate(missingPattern: node)
    case .tuplePattern(let node):
      return self.generate(tuplePattern: node)
    case .valueBindingPattern(let node):
      return self.generate(valueBindingPattern: node).asPattern
    case .wildcardPattern(let node):
      return self.generate(wildcardPattern: node).asPattern
    }
  }

  func generate(expressionPattern node: ExpressionPatternSyntax) -> BridgedExprPattern {
    return .createParsed(
      self.declContext,
      expr: self.generate(expr: node.expression)
    )
  }

  func generate(identifierPattern node: IdentifierPatternSyntax) -> BridgedNamedPattern {
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.identifier)
    self.diagnoseDollarDeclName(node.identifier)
    return .createParsed(
      ctx,
      declContext: declContext,
      name: name,
      loc: nameLoc
    )
  }

  func generate(isTypePattern node: IsTypePatternSyntax) -> BridgedIsPattern {
    return .createParsed(
      self.ctx,
      isLoc: self.generateSourceLoc(node.isKeyword),
      typeExpr: .createParsed(
        self.ctx,
        type: self.generate(type: node.type)
      )
    )
  }

  func generate(missingPattern node: MissingPatternSyntax) -> BridgedPattern {
    // Recover by creating implicit '_' pattern.
    return BridgedAnyPattern.createImplicit(self.ctx).asPattern
  }

  func generate(tuplePattern node: TuplePatternSyntax) -> BridgedPattern {
    if node.elements.count == 1, let firstElement = node.elements.first, firstElement.label == nil {
      return BridgedParenPattern.createParsed(
        self.ctx,
        lParenLoc: self.generateSourceLoc(node.leftParen),
        subPattern: self.generate(pattern: firstElement.pattern),
        rParenLoc: self.generateSourceLoc(node.rightParen)
      ).asPattern
    }
    for element in node.elements {
      if let label = element.label {
        self.diagnoseDollarDeclName(label)
      }
    }
    return BridgedTuplePattern.createParsed(
      self.ctx,
      lParenLoc: self.generateSourceLoc(node.leftParen),
      elements: node.elements.lazy.map {
        BridgedTuplePatternElt(
          Label: self.generateIdentifier($0.label),
          LabelLoc: self.generateSourceLoc($0.label),
          ThePattern: self.generate(pattern: $0.pattern)
        )
      }.bridgedArray(in: self),
      rParenLoc: self.generateSourceLoc(node.rightParen)
    ).asPattern
  }

  func generate(valueBindingPattern node: ValueBindingPatternSyntax) -> BridgedBindingPattern {
    let introducer: BridgedVarDeclIntroducer
    // A misspelled `let case` is parsed as `case let` with a missing specifier,
    // whose text is empty but whose kind is still `let` or `var`.
    let specifierText =
      node.bindingSpecifier.presence == .missing
      ? (node.bindingSpecifier.tokenKind == .keyword(.var) ? "var" : "let")
      : node.bindingSpecifier.text
    switch specifierText {
    case "let": introducer = .let
    case "inout":
      introducer = .inOut
      // Without the feature, the C++ parser does not take 'inout' as a binding
      // specifier in a pattern; it fails parsing the rest as a type.
      if !self.ctx.langOpts.hasFeature(.ReferenceBindings) {
        self.diagnose(.attr_only_on_parameters("inout"), at: node.bindingSpecifier)
      }
    case "borrowing", "_borrowing":
      // `borrowing` in a pattern is deprecated spelling for `let`, but it still
      // gives the bindings the borrowing introducer. It applies only directly to
      // a binding name.
      introducer = .borrowing
      self.diagnose(
        .borrowing_syntax_change,
        at: node.bindingSpecifier,
        fixIts: [.init(replace: self.generateCharSourceRange(node.bindingSpecifier), with: "let")]
      )
      if !node.pattern.is(IdentifierPatternSyntax.self) {
        self.diagnose(.borrowing_subpattern_unsupported, at: node.pattern)
      }
    default: introducer = .var
    }
    self.diagnoseNestedBinding(node, introducer: introducer)
    return .createParsed(
      self.ctx,
      keywordLoc: self.generateSourceLoc(node.bindingSpecifier),
      introducer: introducer,
      subPattern: self.generate(pattern: node.pattern)
    )
  }

  /// Diagnose a 'var', 'let' or 'inout' pattern that is nested inside another
  /// one, or a 'let' pattern in the implicitly immutable pattern of a for-each
  /// loop.
  private func diagnoseNestedBinding(_ node: ValueBindingPatternSyntax, introducer: BridgedVarDeclIntroducer) {
    var isNested = false
    var isImplicitlyImmutable = false
    var child = Syntax(node)
    ancestors: while let parent = child.parent {
      switch parent.as(SyntaxEnum.self) {
      case .valueBindingPattern:
        isNested = true
        break ancestors
      case .patternBinding(let binding):
        // A 'var' or 'let' declaration's own pattern is a binding pattern too.
        if child.id == Syntax(binding.pattern).id {
          isNested = true
        }
        break ancestors
      case .forStmt(let forStmt):
        // The pattern of a for-each loop is immutable unless it is a
        // refutable pattern after 'case'.
        isImplicitlyImmutable = forStmt.caseKeyword == nil && child.id == Syntax(forStmt.pattern).id
        break ancestors
      // Patterns do not extend into closures, declarations, or other
      // statements.
      case .closureExpr, .codeBlockItem, .switchCase, .conditionElement, .catchItem:
        break ancestors
      default:
        if parent.is(DeclSyntax.self) || parent.is(StmtSyntax.self) {
          break ancestors
        }
      }
      child = parent
    }

    // A pattern the parser already found fault with, like `let let::x`, was
    // diagnosed there.
    if isNested, !node.hasError {
      let index: UInt =
        switch introducer {
        case .let, .borrowing: 0
        case .inOut: 1
        case .var: 2
        }
      if self.ctx.langOpts.hasFeature(.ReferenceBindings) {
        self.diagnose(.var_pattern_in_var_inout(index), at: node.bindingSpecifier)
      } else {
        self.diagnose(.var_pattern_in_var(index), at: node.bindingSpecifier)
      }
    }
    if introducer == .let, isImplicitlyImmutable {
      self.diagnose(.let_pattern_in_immutable_context, at: node.bindingSpecifier)
    }
  }

  func generate(wildcardPattern node: WildcardPatternSyntax) -> BridgedAnyPattern {
    return .createParsed(
      self.ctx,
      loc: self.generateSourceLoc(node.wildcard)
    )
  }
}
