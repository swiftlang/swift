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

/// Macro implementing the TaskLocal functionality.
///
/// It introduces a peer `static let $name: TaskLocal<Type>` as well as a getter
/// that accesses the task local storage.
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
    guard try requireStaticContext(varDecl, in: context, diagnose: false) else {
      return []
    }

    guard varDecl.bindings.count == 1 else {
      throw DiagnosticsError(
        syntax: declaration,
        message: "'@TaskLocal' property must have exactly one binding", id: .incompatibleDecl)
    }
    guard let firstBinding = varDecl.bindings.first else {
      throw DiagnosticsError(
        syntax: declaration,
        message: "'@TaskLocal' property must have declared binding", id: .incompatibleDecl)
    }

    guard let name = firstBinding.pattern.as(IdentifierPatternSyntax.self)?.identifier else {
      throw DiagnosticsError(
        syntax: declaration,
        message: "'@TaskLocal' property must have name", id: .incompatibleDecl)
    }

    let originalType = firstBinding.typeAnnotation?.type
    let explicitTypeAnnotation: TypeAnnotationSyntax?
    let taskLocalWrappedType: TypeSyntax?
    if let forceUnwrappedOptionalType = firstBinding.typeAnnotation?.type.as(ImplicitlyUnwrappedOptionalTypeSyntax.self) {
      taskLocalWrappedType = TypeSyntax("\(forceUnwrappedOptionalType.wrappedType.trimmed)?")
    } else {
      taskLocalWrappedType = originalType?.trimmed
    }
    if let taskLocalWrappedType {
      explicitTypeAnnotation = TypeAnnotationSyntax(type: TypeSyntax("TaskLocal<\(taskLocalWrappedType)>"))
    } else {
      explicitTypeAnnotation = nil
    }

    let initialValue: ExprSyntax
    if let initializerValue = firstBinding.initializer?.value {
      initialValue = ExprSyntax(initializerValue)
    } else if let originalType, originalType.isOptional {
      initialValue = ExprSyntax(NilLiteralExprSyntax())
    } else {
      throw DiagnosticsError(
        syntax: declaration,
        message: "'@TaskLocal' property must have default value, or be optional", id: .mustBeVar)
    }

    // Copy access modifiers
    let access = varDecl.accessControlModifiers

    // If the property is global, do not prefix the synthesised decl with 'static'
    let isGlobal = context.lexicalContext.isEmpty
    let staticKeyword: TokenSyntax?
    if isGlobal {
      staticKeyword = nil
    } else {
      staticKeyword = TokenSyntax.keyword(.static, trailingTrivia: .space)
    }

    return [
      """
      \(access)\(staticKeyword)let $\(name)\(explicitTypeAnnotation) = TaskLocal(wrappedValue: \(initialValue))
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
    try requireStaticContext(varDecl, in: context)

    guard let firstBinding = varDecl.bindings.first else {
      return []
    }

    guard let name = firstBinding.pattern.as(IdentifierPatternSyntax.self)?.identifier else {
      return []
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
private func requireStaticContext(_ decl: VariableDeclSyntax,
                                  in context: some MacroExpansionContext,
                                  diagnose: Bool = true) throws -> Bool {
  let isStatic = decl.modifiers.contains { modifier in
    modifier.name.text == "\(Keyword.static)"
  }

  if isStatic {
    return true
  }

  let isGlobal = context.lexicalContext.isEmpty
  if isGlobal {
    return true
  }

  if diagnose {
    throw DiagnosticsError(
      syntax: decl,
      message: "'@TaskLocal' can only be applied to 'static' property, or global variables", id: .mustBeStatic)
  }

  return false
}

extension TypeSyntax {
  // This isn't great since we can't handle type aliases since the macro
  // has no type information, but at least for the common case for Optional<T>
  // and T? we can detect the optional.
  fileprivate var isOptional: Bool {
    switch self.as(TypeSyntaxEnum.self) {
    case .optionalType, .implicitlyUnwrappedOptionalType:
      return true
    case .identifierType(let identifierType):
      return identifierType.name.text == "Optional"
    case .memberType(let memberType):
      guard let baseIdentifier = memberType.baseType.as(IdentifierTypeSyntax.self),
            baseIdentifier.name.text == "Swift" else {
        return false
      }
      return memberType.name.text == "Optional"
    default:
      return false
    }
  }
}

struct TaskLocalMacroDiagnostic: DiagnosticMessage {
  enum ID: String {
    case mustBeVar = "must be var"
    case mustBeStatic = "must be static"
    case incompatibleDecl = "incompatible declaration"
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
  init(
    syntax: some SyntaxProtocol,
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
