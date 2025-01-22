//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftDiagnostics

//===----------------------------------------------------------------------===//
// MARK: Decl diagnostics
//===----------------------------------------------------------------------===//

extension ASTGenDiagnostic {
  static func nonTrivialPatternForAccessor(_ pattern: some SyntaxProtocol) -> Self {
    Self(
      node: pattern,
      message: "getter/setter can only be defined for a single variable"
    )
  }

  static func unknownAccessorSpecifier(_ specifier: TokenSyntax) -> Self {
    Self(
      node: specifier,
      message: "unknown accessor specifier '\(specifier.text)'"
    )
  }
}

//===----------------------------------------------------------------------===//
// MARK: Decl attribute diagnostics
//===----------------------------------------------------------------------===//

extension ASTGenDiagnostic {
  static func expectedArgumentsInAttribute(_ attribute: AttributeSyntax) -> Self {
    Self(
      node: attribute,
      position: attribute.rightParen?.positionAfterSkippingLeadingTrivia ??  attribute.endPositionBeforeTrailingTrivia,
      message: "expected arguments in '@\(attribute.attributeName.trimmedDescription)' attribute"
    )
  }

  static func unexpectedArgumentInAttribute(_ attribute: AttributeSyntax, _ extra: some SyntaxProtocol) -> Self {
    Self(
      node: extra,
      message: "unexpected arguments in '@\(attribute.attributeName.trimmedDescription)' attribute"
    )
  }

  static func expectedOptionForAttribute(_ attribute: AttributeSyntax, suchAs example: String) -> Self {
    Self(
      node: attribute,
      position: attribute.rightParen?.positionAfterSkippingLeadingTrivia ??  attribute.endPositionBeforeTrailingTrivia,
      message: "expected option in '@\(attribute.attributeName)' attribute such as '\(example)'"
    )
  }

  static func expectedIdentifierOptionForAttribute(_ attribute: AttributeSyntax, at arg: some SyntaxProtocol) -> Self {
    Self(
      node: arg,
      message: "expected an identifier in '@\(attribute.attributeName)' attribute"
    )
  }

  static func expectedArgumentLabelInAttribute(_ attribute: AttributeSyntax, label: String, at arg: some SyntaxProtocol) -> Self {
    Self(
      node: arg,
      message: "expected '\(label)' argument in '@\(attribute.attributeName)' attribute"
    )
  }

  static func expectedArgumentValueInAttribute(_ attribute: AttributeSyntax, label: String, value: String, at arg: some SyntaxProtocol) -> Self {
    Self(
      node: arg,
      message: "expected \(value) for '\(label)' argument in '@\(attribute.attributeName)' attribute"
    )
  }


  static func unexpectedArgumentLabelInAttribute(_ attribute: AttributeSyntax, at arg: LabeledExprSyntax) -> Self {
    Self(
      node: arg,
      message: "unexpected argument label '\(arg.label!):' in '@\(attribute.attributeName)' attribute"
    )
  }

  static func unexpectedArgumentsTypeInAttribute(_ attribute: AttributeSyntax, arguments: AttributeSyntax.Arguments, expected: any SyntaxProtocol.Type) -> Self {
    Self(
      node: attribute.arguments!,
      message: "(compiler bug) unexpected argument type for @\(attribute.attributeName): \(arguments.kind.syntaxNodeType), expected: \(expected)"
    )
  }

  static func duplicatedLabeledArgumentInAttribute(_ attribute: AttributeSyntax, argument: some SyntaxProtocol, name: String) -> Self {
    Self(
      node: argument,
      message: "duplicated argument '\(name)' in '@\(attribute.attributeName)' attribute"
    )
  }

  static func stringInterpolationNotAllowedInAttribute(_ attribute: AttributeSyntax, at node: some ExprSyntaxProtocol) -> Self {
    Self(
      node: Syntax(node),
      message: "string interpolation is not accepted in '@\(attribute.attributeName)' attribute"
    )
  }
}

//===----------------------------------------------------------------------===//
// MARK: Availability diagnostics
//===----------------------------------------------------------------------===//

extension ASTGenDiagnostic {
  static func expectedVersionNumberInAvailableAttr(_ node: AvailabilityLabeledArgumentSyntax) -> Self {
    Self(
      node: node.value,
      message: "expected version number for '\(node.label.text):' argument"
    )
  }

  static func expectedVersionNumberAfterPlatform(_ node: TokenSyntax) -> Self {
    Self(
      node: node,
      position: node.endPositionBeforeTrailingTrivia,
      message: "expected version number after '\(node.text)'"
    )
  }

  static func unexpectedArgumentInAvailabilitySpecList(_ node: TokenSyntax) -> Self {
    Self(
      node: node,
      message: "expected platform name"
    )
  }
}

//===----------------------------------------------------------------------===//
// MARK: Type diagnostics
//===----------------------------------------------------------------------===//

extension ASTGenDiagnostic {
  static func classKeywordInheritanceDeprecated(_ node: ClassRestrictionTypeSyntax) -> Self {
    Self(
      node: node,
      message: "using 'class' keyword to define a class-constrained protocol is deprecated",
      severity: .warning
    )
    .withFixIt(
      message: "use 'AnyObject' instead",
      changes: .replaceTokenText(node.classKeyword, with: .identifier("AnyObject"))
    )
  }
}

//===----------------------------------------------------------------------===//
// MARK: Misc diagnostics
//===----------------------------------------------------------------------===//

extension ASTGenDiagnostic {
  static func illegalTopLevelStmt(_ stmt: some SyntaxProtocol) -> Self {
    Self(
      node: stmt,
      message: "statements are not allowed at the top level"
    )
  }

  static func illegalTopLevelExpr(_ expr: some SyntaxProtocol) -> Self {
    Self(
      node: expr,
      message: "expressions are not allowed at the top level"
    )
  }

  static func poundDiagnostic(_ node: StringLiteralExprSyntax, message: String, isError: Bool) -> Self {
    Self(
      node: node,
      message: node.representedLiteralValue!,
      severity: isError ? .error : .warning
    )
  }
}

extension ASTGenDiagnostic {
  /// An error emitted when a token is of an unexpected kind.
  static func unexpectedTokenKind(token: TokenSyntax) -> Self {
    guard let parent = token.parent else {
      preconditionFailure("Expected a child (not a root) token")
    }

    return Self(
      node: token,
      message: """
      unexpected token kind for token:
        \(token.debugDescription)
      in parent:
        \(parent.debugDescription(indentString: "  "))
      """
    )
  }

  /// An error emitted when an optional child token is unexpectedly nil.
  static func missingChildToken(parent: some SyntaxProtocol, kindOfTokenMissing: TokenKind) -> Self {
    Self(
      node: parent,
      message: """
      missing child token of kind '\(kindOfTokenMissing)' in:
        \(parent.debugDescription(indentString: "  "))
      """
    )
  }

  /// An error emitted when a syntax collection entry is encountered that is
  /// considered a duplicate of a previous entry per the language grammar.
  static func duplicateSyntax(duplicate: some SyntaxProtocol, original: some SyntaxProtocol) -> Self {
    precondition(duplicate.kind == original.kind, "Expected duplicate and original to be of same kind")

    guard let duplicateParent = duplicate.parent, let originalParent = original.parent,
      duplicateParent == originalParent, duplicateParent.kind.isSyntaxCollection
    else {
      preconditionFailure("Expected a shared syntax collection parent")
    }

    return Self(
      node: duplicate,
      message: """
      unexpected duplicate syntax in list:
        \(duplicate.debugDescription(indentString: "  "))
      previous syntax:
        \(original.debugDescription(indentString: "  "))
      """
    )
  }
}

//===----------------------------------------------------------------------===//
// MARK: Convenient message construction methods
//===----------------------------------------------------------------------===//

extension ASTGenDiagnostic {
  fileprivate init(node: some SyntaxProtocol, position: AbsolutePosition? = nil, message: String, severity: DiagnosticSeverity = .error, function: String = #function) {
    // Derive messageID from the function name.
    let messageID = String(function.prefix(while: { $0 != "(" }))
    self.init(id: messageID, node: Syntax(node), position: position, message: message, severity: severity)
  }

  fileprivate func withNote(node: some SyntaxProtocol, message: String) -> Self {
    let messageID = "\(self.diagnosticID)_node\(self.fixIts.count)"
    var copy = self
    copy.notes.append(
      Note(node: Syntax(node), message: message, messageID: messageID)
    )
    return copy
  }

  fileprivate func withFixIt(message: String, changes: FixIt.Change...) -> Self {
    let messageID = "\(self.diagnosticID)_fixit\(self.fixIts.count)"
    var copy = self
    copy.fixIts.append(
      FixIt(message: message, messageID: messageID, changes: changes)
    )
    return copy
  }
}
