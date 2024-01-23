//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxMacros
import SwiftDiagnostics
import SwiftOperators
import SwiftSyntaxBuilder

protocol DistributedMacro: FreestandingMacro {}

extension DistributedMacro {

  static func paramIndexInit() -> Int { -1 }
  static func typeParamIndexInit() -> Int { -1 }

  static func takeArgument(named name: String, at paramIdx: inout Int, node: some FreestandingMacroExpansionSyntax) throws -> LabeledExprSyntax {
    paramIdx += 1

    guard let param = node.arguments.dropFirst(paramIdx).first else {
      throw DiagnosticsError(syntax: node, message: "Missing '\(name)' argument", id: .missingArgument)
    }

    return param
  }

  static func takeModuleArgument(at paramIdx: inout Int, node: some FreestandingMacroExpansionSyntax) throws -> LabeledExprSyntax {
    try takeArgument(named: "module", at: &paramIdx, node: node)
  }

  static func takeIdArgument(at paramIdx: inout Int, node: some FreestandingMacroExpansionSyntax) throws -> LabeledExprSyntax {
    try takeArgument(named: "id", at: &paramIdx, node: node)
  }

  static func takeSystemArgument(at paramIdx: inout Int, node: some FreestandingMacroExpansionSyntax) throws -> LabeledExprSyntax {
    try takeArgument(named: "system", at: &paramIdx, node: node)
  }

  static func takeProtocolNameArgument(at paramIdx: inout Int, node: some FreestandingMacroExpansionSyntax) throws -> LabeledExprSyntax {
    try takeArgument(named: "protocolName", at: &paramIdx, node: node)
  }
  static func takeStubProtocols(at paramIdx: inout Int, node: some FreestandingMacroExpansionSyntax) throws -> [String] {
    let array = try takeArgument(named: "protocolName", at: &paramIdx, node: node)

    var names: [String] = []
    // TODO: reserve capacity

    guard let labeledExpr = array.as(LabeledExprSyntax.self) else {
      fatalError("Cannot parse syntax as \(LabeledExprSyntax.self), was: \(array.debugDescription)")
    }

    for text in labeledExpr.children(viewMode: .all) {
      guard let text = text.as(StringLiteralExprSyntax.self) else {
        continue
      }
      for segment in text.segments {
        names.append("\(segment)")
      }
    }

    return names
  }

  static func takeSystemNameArgument(at paramIdx: inout Int, node: some FreestandingMacroExpansionSyntax) throws -> StringLiteralSegmentListSyntax.Element {
    paramIdx += 1
    guard let systemNameParam = node.arguments.dropFirst(paramIdx).first else {
      throw DiagnosticsError(syntax: node, message: "Missing 'systemName' argument", id: .missingArgument)
    }
    guard let systemNameLiteral = systemNameParam.expression.as(StringLiteralExprSyntax.self) else {
      throw DiagnosticsError(syntax: node, message: "Expected 'systemName' to be a string literal, was: \(systemNameParam.expression)", id: .illegalSyntaxNode)
    }
    guard let systemName = systemNameLiteral.segments.first else {
      throw DiagnosticsError(syntax: node, message: "Expected 'systemName' to be a string literal 1 segment, was: \(systemNameLiteral.debugDescription)", id: .illegalSyntaxNode)
    }

    return systemName
  }
}

// ==== ------------------------------------------------------------------------

extension TypeSyntax {
  var identifier: String? {
    for token in tokens(viewMode: .all) {
      switch token.tokenKind {
      case .identifier(let identifier):
        return identifier
      default:
        break
      }
    }
    return nil
  }
}

extension TokenSyntax {
  var identifier: String? {
    for token in tokens(viewMode: .all) {
      switch token.tokenKind {
      case .identifier(let identifier):
        return identifier
      default:
        break
      }
    }
    return nil
  }
}

extension MemberBlockItemSyntax {
  var memberIdentifier: String? {
    // FIXME: this assumes a func

    if let funcDecl = self.decl.as(FunctionDeclSyntax.self) {
    let parameters = funcDecl.signature.parameterClause.parameters
      guard !parameters.isEmpty else {
        return "\(funcDecl.name)()"
      }

      var parametersString: String =
        parameters
          .map { ($0.firstName.identifier ?? "_") + ":" }
          .joined(separator: "")
      print("parametersString == \(parametersString)")
      return "\(funcDecl.name)(\(parametersString))"

    } else {
      fatalError("Cannot get name of: \(self)")
    }

//    for token in tokens(viewMode: .all) {
//      switch token.tokenKind {
//      case .name(let name):
//        return name.trimmed
//      default:
//        break
//      }
//    }
//    return nil
  }
}