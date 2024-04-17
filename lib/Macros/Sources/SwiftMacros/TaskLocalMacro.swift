//===----------------------------------------------------------------------===//
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

import SwiftSyntax
import SwiftSyntaxMacros
import SwiftDiagnostics

// TODO: docs
public enum TaskLocalMacro {}

extension TaskLocalMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let varDecl = try requireVar(declaration, diagnose: false) else {
      return []
    }
    guard try requireModifier(varDecl, .static, diagnose: false) else {
      return []
    }

    guard let firstBinding = varDecl.bindings.first else {
      return [] // TODO: make error
    }

    guard let name = firstBinding.pattern.as(IdentifierPatternSyntax.self)?.identifier else {
      return [] // TODO: make error
    }

    let type = firstBinding.typeAnnotation?.type
    let explicitType: String =
      if let type {
        ": TaskLocal<\(type.trimmed)>"
      } else {
        ""
      }

    let initialValue: Any
    if let initializerValue = firstBinding.initializer?.value {
      initialValue = initializerValue
    } else if let type, type.isOptional {
      initialValue = "nil"
    } else {
      throw DiagnosticsError(
        syntax: declaration,
        message: "'@TaskLocal' property must have default value, or be optional", id: .mustBeVar)
    }

    return [
      """
      static let $\(name)\(raw: explicitType) = TaskLocal(wrappedValue: \(raw: initialValue))
      """
    ]
  }
}

extension TaskLocalMacro: AccessorMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingAccessorsOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [AccessorDeclSyntax] {
    // We very specifically have to fail and diagnose in the accessor macro,
    // rather than in the peer macro, since returning [] from the accessor
    // macro adds another series of errors about it missing to emit a decl.
    guard let varDecl = try requireVar(declaration) else {
      return []
    }
    try requireModifier(varDecl, .static)

    guard let firstBinding = varDecl.bindings.first else {
      return [] // TODO: make error
    }

    guard let name = firstBinding.pattern.as(IdentifierPatternSyntax.self)?.identifier else {
      return [] // TODO: make error
    }

    return ["get { $\(name).get() }"]
  }
}

@discardableResult
private func requireVar(_ decl: some DeclSyntaxProtocol,
                        diagnose: Bool = true) throws -> VariableDeclSyntax? {
  if let varDecl = decl.as(VariableDeclSyntax.self) {
    return varDecl
  }
  if diagnose {
    throw DiagnosticsError(
      syntax: decl,
      message: "'@TaskLocal' can only be applied to properties", id: .mustBeVar)
  }

  return nil
}

@discardableResult
private func requireModifier(_ decl: VariableDeclSyntax,
                             _ keyword: Keyword,
                             diagnose: Bool = true) throws -> Bool {
  let isStatic = decl.modifiers.contains { modifier in
    modifier.name.text == "\(keyword)"
  }

  if !isStatic {
    if diagnose {
      throw DiagnosticsError(
        syntax: decl,
        message: "'@TaskLocal' can only be applied to 'static' property", id: .mustBeStatic)
    } else {
      return false
    }
  }

  return true
}

extension TypeSyntax {
  // This isn't great since we can't handle type aliases since the macro
  // has no type information, but at least for the common case for Optional<T>
  // and T? we can detect the optional.
  fileprivate var isOptional: Bool {
    let strRepr = "\(self)"
    return strRepr.last == "?" ||
      strRepr.starts(with: "Optional<") ||
      strRepr.starts(with: "Swift.Optional<")
  }
}

struct TaskLocalMacroDiagnostic: DiagnosticMessage {
  enum ID: String {
    case mustBeStatic = "must be static"
    case mustBeVar = "must be var"
  }

  var message: String
  var diagnosticID: MessageID
  var severity: DiagnosticSeverity

  init(message: String, diagnosticID: SwiftDiagnostics.MessageID, severity: SwiftDiagnostics.DiagnosticSeverity = .error) {
    self.message = message
    self.diagnosticID = diagnosticID
    self.severity = severity
  }

  init(message: String, domain: String, id: ID, severity: SwiftDiagnostics.DiagnosticSeverity = .error) {
    self.message = message
    self.diagnosticID = MessageID(domain: domain, id: id.rawValue)
    self.severity = severity
  }
}

extension DiagnosticsError {
  init<S: SyntaxProtocol>(
    syntax: S,
    message: String,
    domain: String = "Swift",
    id: TaskLocalMacroDiagnostic.ID,
    severity: SwiftDiagnostics.DiagnosticSeverity = .error) {
    self.init(diagnostics: [
      Diagnostic(
        node: Syntax(syntax),
        message: TaskLocalMacroDiagnostic(
          message: message,
          domain: domain,
          id: id,
          severity: severity))
    ])
  }
}
