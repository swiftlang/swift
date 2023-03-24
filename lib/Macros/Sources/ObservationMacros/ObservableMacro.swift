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

@_implementationOnly import SwiftDiagnostics
@_implementationOnly import SwiftOperators
@_implementationOnly import SwiftSyntaxBuilder

public struct ObservableMacro {
  static let registarVariableName = "_$observationRegistrar"
  static let storageVariableName = "_$observationStorage"
  static let storageTypeName = "_$ObservationStorage"
}

extension ObservableMacro: MemberMacro {
  public static func expansion<
    Declaration: DeclGroupSyntax,
    Context: MacroExpansionContext
  >(
    of node: AttributeSyntax,
    providingMembersOf declaration: Declaration,
    in context: Context
  ) throws -> [DeclSyntax] {
    guard let identified = declaration.asProtocol(IdentifiedDeclSyntax.self) else {
      return []
    }
    
    let parentName = identified.identifier
    
    let registrar: DeclSyntax =
      """
      let \(raw: registarVariableName) = ObservationRegistrar<\(parentName)>()
      """

    let changes: DeclSyntax =
      """
      public nonisolated func changes<Isolation: Actor>(
        for properties: TrackedProperties<\(parentName)>,
        isolatedTo isolation: Isolation
      ) -> ObservedChanges<\(parentName), Isolation> {
        \(raw: registarVariableName).changes(
          for: properties, 
          isolatedTo: isolation
        )
      }
      """

    let values: DeclSyntax =
      """
      public nonisolated func values<Member: Sendable>(
        for keyPath: KeyPath<\(parentName), Member>
      ) -> ObservedValues<\(parentName), Member> {
        \(raw: registarVariableName).values(for: keyPath)
      }
      """
    
    let memberFields: [(IdentifierPatternSyntax, TypeSyntax, InitializerClauseSyntax?)] =
      try declaration.members.members.compactMap { member in
        if let variableDecl = member.as(MemberDeclListItemSyntax.self)?.decl.as(VariableDeclSyntax.self) {
          
          if let identifierPattern = variableDecl.bindings.first?.pattern.as(IdentifierPatternSyntax.self) {
            if let typeAnnotation = variableDecl.bindings.first?.typeAnnotation {
              return (identifierPattern, typeAnnotation.type, variableDecl.bindings.first?.initializer)
            } else {
              throw DiagnosticsError(
                diagnostics: [
                  Diagnostic(node: Syntax(variableDecl), message: MissingTypeAnnotationMessage(identifierPattern))
                ]
              )
            }
          }
        }
        return nil
      }
    
    
    let storageStruct = StructDeclSyntax(
      identifier: TokenSyntax(
        .identifier(storageTypeName), 
        leadingTrivia: .space, 
        presence: .present
      )
    ) {
      for (identifierPattern, type, _) in memberFields {
        VariableDeclSyntax(bindingKeyword: .keyword(.var)) {
          PatternBindingSyntax(
            leadingTrivia: .space,
            pattern: identifierPattern,
            typeAnnotation: TypeAnnotationSyntax(colon: .colonToken(), type: type),
            trailingTrivia: .newline
          )
        }
      }
    }
    
    let parameterInitialization = memberFields.map { (identifierPattern, _, _) in
      return "\(identifierPattern.identifier.text): \(identifierPattern.identifier.text)"
    }.joined(separator: ",")
    
    let initializer = InitializerDeclSyntax(
        signature: FunctionSignatureSyntax(
          input: ParameterClauseSyntax(
            parameterList: FunctionParameterListSyntax {
              for (identifierPattern, type, initializer) in memberFields {
                FunctionParameterSyntax(
                  firstName: identifierPattern.identifier,
                  colon: .colonToken(),
                  type: type,
                  defaultArgument: initializer
                )
              }
            }
          )
        )
    ) {
      ExprSyntax(
      """
      \(raw: storageVariableName) = \(raw: storageTypeName)(\(raw: parameterInitialization))
      """
      )
    }
    let storage: DeclSyntax =
      """
      private var \(raw: storageVariableName): \(raw: storageTypeName)
      """

    return [
      registrar,
      changes,
      values,
      DeclSyntax(storageStruct),
      storage,
      DeclSyntax(initializer)
    ]
  }
}

extension ObservableMacro: MemberAttributeMacro {
  public static func expansion<
    Declaration: DeclGroupSyntax,
    MemberDeclaration: DeclSyntaxProtocol,
    Context: MacroExpansionContext
  >(
    of node: AttributeSyntax,
    attachedTo declaration: Declaration,
    providingAttributesFor member: MemberDeclaration,
    in context: Context
  ) throws -> [AttributeSyntax] {
    guard let property = member.as(VariableDeclSyntax.self) else {
      return []
    }
    guard let binding = property.bindings.first else {
      return []
    }
    
    guard let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier else {
      return []
    }
    if identifier.text == ObservableMacro.registarVariableName || identifier.text == ObservableMacro.storageVariableName { return [] }

    return [
      AttributeSyntax(
        attributeName: SimpleTypeIdentifierSyntax(
          name: .identifier("ObservableProperty")
        )
      )
    ]
  }
}

struct InvalidActorApplicationMessage: DiagnosticMessage {
  let actorDecl: ActorDeclSyntax
  
  var message: String {
    "@Observable applied to actor \(actorDecl.identifier.text) is not supported"
  }
  
  var diagnosticID: SwiftDiagnostics.MessageID { SwiftDiagnostics.MessageID(domain: "Observable", id: "actor conformance")}
  
  let severity: SwiftDiagnostics.DiagnosticSeverity = .error
  
  init(_ actorDecl: ActorDeclSyntax) {
    self.actorDecl = actorDecl
  }
}

struct MissingTypeAnnotationMessage: DiagnosticMessage {
  let identifier: IdentifierPatternSyntax
  
  var message: String {
    "@Observable requires properties to have type annotations. \(identifier.identifier.text) is missing a type"
  }
  
  var diagnosticID: SwiftDiagnostics.MessageID { SwiftDiagnostics.MessageID(domain: "Observable", id: "missing type")}
  
  let severity: SwiftDiagnostics.DiagnosticSeverity = .error
  
  init(_ identifier: IdentifierPatternSyntax) {
    self.identifier = identifier
  }
}

extension ObservableMacro: ConformanceMacro {
    public static func expansion<
      Declaration: DeclGroupSyntax,
      Context: MacroExpansionContext
    >(
      of node: AttributeSyntax,
      providingConformancesOf declaration: Declaration,
      in context: Context
    ) throws -> [(TypeSyntax, GenericWhereClauseSyntax?)] {
      let identified = declaration.asProtocol(IdentifiedDeclSyntax.self)
      if let actorDecl = identified as? ActorDeclSyntax {
        throw DiagnosticsError(
          diagnostics: [
            Diagnostic(node: Syntax(actorDecl), message: InvalidActorApplicationMessage(actorDecl))
          ]
        )
      }
      
      let protocolName: TypeSyntax = "Observable"
      return [(protocolName, nil)]
    }
}

struct DebugMessage: DiagnosticMessage {
  var message: String
  
  var diagnosticID: SwiftDiagnostics.MessageID { .init(domain: "test", id: "test")}
  
  let severity: SwiftDiagnostics.DiagnosticSeverity
  
  init(message: String, severity: SwiftDiagnostics.DiagnosticSeverity = .error) {
    self.message = message
    self.severity = severity
  }
  
  func emit<S: SyntaxProtocol>(_ node: S) throws {
    throw DiagnosticsError(diagnostics: [Diagnostic(node: Syntax(node), message: self)])
  }
}

extension VariableDeclSyntax {
  func accessorsMatching(_ predicate: (TokenKind) -> Bool) -> [AccessorDeclSyntax] {
    let patternBindings = bindings.compactMap { binding in
      binding.as(PatternBindingSyntax.self)
    }
    let accessors = patternBindings.compactMap { patternBinding in
      switch patternBinding.accessor {
      case .accessors(let accessors):
        return accessors
      default:
        return nil
      }
    }.flatMap { $0.accessors }
    return accessors.compactMap { accessor in
      guard let decl = accessor.as(AccessorDeclSyntax.self) else {
        return nil
      }
      if predicate(decl.accessorKind.tokenKind) {
        return decl
      } else {
        return nil
      }
    }
  }
  
  var willSetAccessors: [AccessorDeclSyntax] {
    accessorsMatching { $0 == .keyword(.willSet) }
  }
  var didSetAccessors: [AccessorDeclSyntax] {
    accessorsMatching { $0 == .keyword(.didSet) }
  }
}

public struct ObservablePropertyMacro: AccessorMacro {
  public static func expansion<
    Context: MacroExpansionContext,
    Declaration: DeclSyntaxProtocol
  >(
    of node: AttributeSyntax,
    providingAccessorsOf declaration: Declaration,
    in context: Context
  ) throws -> [AccessorDeclSyntax] {
    guard let property = declaration.as(VariableDeclSyntax.self),
      let binding = property.bindings.first,
      let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier
    else {
      return []
    }
    
    if identifier.text == ObservableMacro.registarVariableName || 
       identifier.text == ObservableMacro.storageVariableName { 
      return [] 
    }

    let prolog = CodeBlockItemListSyntax(
      property.willSetAccessors.compactMap { $0.body }.flatMap { $0.statements }
    )
    let epilog = CodeBlockItemListSyntax(
      property.didSetAccessors.compactMap { $0.body }.flatMap { $0.statements }
    )
    
    let getAccessor: AccessorDeclSyntax =
      """
      get {
        \(raw: ObservableMacro.registarVariableName).access(self, keyPath: \\.\(identifier))
        return \(raw: ObservableMacro.storageVariableName).\(identifier)
      }
      """

    let setAccessor: AccessorDeclSyntax =
      """
      set {
        \(prolog)
        \(raw: ObservableMacro.registarVariableName).withMutation(of: self, keyPath: \\.\(identifier)) {
          \(raw: ObservableMacro.storageVariableName).\(identifier) = newValue
        }
        \(epilog)
      }
      """

    return [getAccessor, setAccessor]
  }
}
