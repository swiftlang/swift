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
  static let moduleName = "Observation"

  static let conformanceName = "Observable"
  static var qualifiedConformanceName: String {
    return "\(moduleName).\(conformanceName)"
  }

  static var observableConformanceType: TypeSyntax {
    "\(raw: qualifiedConformanceName)"
  }

  static let registrarTypeName = "ObservationRegistrar"
  static var qualifiedRegistrarTypeName: String {
    return "\(moduleName).\(registrarTypeName)"
  }
  
  static let trackedMacroName = "ObservationTracked"
  static let ignoredMacroName = "ObservationIgnored"
  static let valuesMacroName = "ObservableValues"

  static let valuesName = "ObservedValues"
  static var qualifiedValuesTypeName: String {
    return "\(moduleName).\(valuesName)"
  }

  static let registrarVariableName = "_$observationRegistrar"
  
  static func registrarVariable(_ observableType: TokenSyntax) -> DeclSyntax {
    return
      """
      @\(raw: ignoredMacroName) private let \(raw: registrarVariableName) = \(raw: qualifiedRegistrarTypeName)()
      """
  }
  
  static func accessFunction(_ observableType: TokenSyntax) -> DeclSyntax {
    return
      """
      internal nonisolated func access<Member>(
          keyPath: KeyPath<\(observableType), Member>
      ) {
        \(raw: registrarVariableName).access(self, keyPath: keyPath)
      }
      """
  }
  
  static func withMutationFunction(_ observableType: TokenSyntax, structural: Bool) -> DeclSyntax {
    return
      """
      internal \(raw: structural ? "mutating " : "")nonisolated func withMutation<Member, T>(
        keyPath: KeyPath<\(observableType), Member>,
        _ mutation: () throws -> T
      ) rethrows -> T {
        try \(raw: registrarVariableName).\(raw: structural ? "withUniqueMutation" : "withMutation")(of: self, keyPath: keyPath, mutation)
      }
      """
  }
  
  static var ignoredAttribute: AttributeSyntax {
    AttributeSyntax(
      leadingTrivia: .space,
      atSignToken: .atSignToken(),
      attributeName: SimpleTypeIdentifierSyntax(name: .identifier(ignoredMacroName)),
      trailingTrivia: .space
    )
  }
  
  static var valuesAttribute: AttributeSyntax {
    AttributeSyntax(
      leadingTrivia: .space,
      atSignToken: .atSignToken(),
      attributeName: SimpleTypeIdentifierSyntax(name: .identifier(valuesMacroName)),
      trailingTrivia: .space
    )
  }
}

struct ObservationDiagnostic: DiagnosticMessage {
  enum ID: String {
    case invalidApplication = "invalid type"
    case missingInitializer = "missing initializer"
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
  init<S: SyntaxProtocol>(syntax: S, message: String, domain: String = "Observation", id: ObservationDiagnostic.ID, severity: SwiftDiagnostics.DiagnosticSeverity = .error) {
    self.init(diagnostics: [
      Diagnostic(node: Syntax(syntax), message: ObservationDiagnostic(message: message, domain: domain, id: id, severity: severity))
    ])
  }
}

extension ModifierListSyntax {
  func privatePrefixed(_ prefix: String) -> ModifierListSyntax {
    let modifier: DeclModifierSyntax = DeclModifierSyntax(name: "private", trailingTrivia: .space)
    return ModifierListSyntax([modifier] + filter {
      switch $0.name.tokenKind {
      case .keyword(let keyword):
        switch keyword {
        case .fileprivate: fallthrough
        case .private: fallthrough
        case .internal: fallthrough
        case .public:
          return false
        default:
          return true
        }
      default:
        return true
      }
    })
  }
  
  init(keyword: Keyword) {
    self.init([DeclModifierSyntax(name: .keyword(keyword))])
  }
}

extension TokenSyntax {
  func privatePrefixed(_ prefix: String) -> TokenSyntax {
    switch tokenKind {
    case .identifier(let identifier):
      return TokenSyntax(.identifier(prefix + identifier), leadingTrivia: leadingTrivia, trailingTrivia: trailingTrivia, presence: presence)
    default:
      return self
    }
  }
}

extension PatternBindingListSyntax {
  func privatePrefixed(_ prefix: String) -> PatternBindingListSyntax {
    var bindings = self.map { $0 }
    for index in 0..<bindings.count {
      let binding = bindings[index]
      if let identifier = binding.pattern.as(IdentifierPatternSyntax.self) {
        bindings[index] = PatternBindingSyntax(
          leadingTrivia: binding.leadingTrivia,
          pattern: IdentifierPatternSyntax(
            leadingTrivia: identifier.leadingTrivia,
            identifier: identifier.identifier.privatePrefixed(prefix),
            trailingTrivia: identifier.trailingTrivia
          ),
          typeAnnotation: binding.typeAnnotation,
          initializer: binding.initializer,
          accessor: binding.accessor,
          trailingComma: binding.trailingComma,
          trailingTrivia: binding.trailingTrivia)
        
      }
    }
    
    return PatternBindingListSyntax(bindings)
  }
}

extension VariableDeclSyntax {
  func privatePrefixed(_ prefix: String, addingAttribute attribute: AttributeSyntax, removingAttribute toRemove: AttributeSyntax? = nil) -> VariableDeclSyntax {
    let filtered = attributes?.filter {
      switch $0 {
      case .attribute(let attr):
        if attr.trimmed.attributeName.identifier == attribute.trimmed.attributeName.identifier {
          return false
        } else if let toRemove, attr.trimmed.attributeName.identifier == toRemove.trimmed.attributeName.identifier {
          return false
        } else {
          return true
        }
      default:
        return true
      }
    } ?? []
    
    
    return VariableDeclSyntax(
      leadingTrivia: leadingTrivia,
      attributes: AttributeListSyntax(filtered + [.attribute(attribute)]),
      modifiers: modifiers?.privatePrefixed(prefix) ?? ModifierListSyntax(keyword: .private),
      bindingKeyword: TokenSyntax(bindingKeyword.tokenKind, leadingTrivia: .space, trailingTrivia: .space, presence: .present),
      bindings: bindings.privatePrefixed(prefix),
      trailingTrivia: trailingTrivia
    )
  }
  
  var isValidForObservation: Bool {
    !isComputed && isInstance && !isImmutable && identifier != nil
  }
  
  var isValidForValueObservation: Bool {
    isInstance && !isImmutable && identifier != nil
  }
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
    
    let observableType = identified.identifier
    
    if declaration.isEnum {
      // enumerations cannot store properties
      throw DiagnosticsError(syntax: node, message: "@Observable cannot be applied to enumeration type \(observableType.text)", id: .invalidApplication)
    }
    if declaration.isActor {
      // actors cannot yet be supported for their isolation
      throw DiagnosticsError(syntax: node, message: "@Observable cannot be applied to actor type \(observableType.text)", id: .invalidApplication) 
    }
    
    var declarations = [DeclSyntax]()

    declaration.addIfNeeded(ObservableMacro.registrarVariable(observableType), to: &declarations)
    declaration.addIfNeeded(ObservableMacro.accessFunction(observableType), to: &declarations)
    declaration.addIfNeeded(ObservableMacro.withMutationFunction(observableType, structural: declaration.isStruct), to: &declarations)
    
    return declarations
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
    guard let property = member.as(VariableDeclSyntax.self), property.isValidForObservation,
          property.identifier != nil else {
      return []
    }

    // dont apply to ignored properties or properties that are already flaged as tracked
    if property.hasMacroApplication(ObservableMacro.ignoredMacroName) {
      return []
    }
    
    
    return [
      AttributeSyntax(attributeName: SimpleTypeIdentifierSyntax(name: .identifier(ObservableMacro.trackedMacroName)))
    ]
  }
}

extension ObservableMacro: ConformanceMacro {
  public static func expansion<Declaration: DeclGroupSyntax, Context: MacroExpansionContext>(
    of node: AttributeSyntax,
    providingConformancesOf declaration: Declaration,
    in context: Context
  ) throws -> [(TypeSyntax, GenericWhereClauseSyntax?)] {
    let inheritanceList: InheritedTypeListSyntax?
    if let classDecl = declaration.as(ClassDeclSyntax.self) {
      inheritanceList = classDecl.inheritanceClause?.inheritedTypeCollection
    } else if let structDecl = declaration.as(StructDeclSyntax.self) {
      inheritanceList = structDecl.inheritanceClause?.inheritedTypeCollection
    } else {
      inheritanceList = nil
    }
    
    if let inheritanceList {
      for inheritance in inheritanceList {
        if inheritance.typeName.identifier == ObservableMacro.conformanceName {
          return []
        }
      }
    }
    
    return [(ObservableMacro.observableConformanceType, nil)]
  }
}

public struct ObservationTrackedMacro {}

extension ObservationTrackedMacro: AccessorMacro {
  public static func expansion<
    Context: MacroExpansionContext,
    Declaration: DeclSyntaxProtocol
  >(
    of node: AttributeSyntax,
    providingAccessorsOf declaration: Declaration,
    in context: Context
  ) throws -> [AccessorDeclSyntax] {
    guard let property = declaration.as(VariableDeclSyntax.self),
          property.isValidForObservation,
          let identifier = property.identifier else {
      return []
    }

    if property.hasMacroApplication(ObservableMacro.ignoredMacroName) {
      return []
    }

    let initAccessor: AccessorDeclSyntax =
      """
      init(initialValue) initializes(_\(identifier)) {
        _\(identifier) = initialValue
      }
      """

    let getAccessor: AccessorDeclSyntax =
      """
      get {
        access(keyPath: \\.\(identifier))
        return _\(identifier)
      }
      """

    let setAccessor: AccessorDeclSyntax =
      """
      set {
        withMutation(keyPath: \\.\(identifier)) {
          _\(identifier) = newValue
        }
      }
      """

    return [initAccessor, getAccessor, setAccessor]
  }
}

extension ObservationTrackedMacro: PeerMacro {
  public static func expansion(
    of node: SwiftSyntax.AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let property = declaration.as(VariableDeclSyntax.self),
          property.isValidForObservation else {
      return []
    }

    if property.hasMacroApplication(ObservableMacro.ignoredMacroName) ||
       property.hasMacroApplication(ObservableMacro.trackedMacroName) || 
       property.hasMacroApplication(ObservableMacro.valuesMacroName) {
      return []
    }
    
    let storage = DeclSyntax(property.privatePrefixed("_", addingAttribute: ObservableMacro.ignoredAttribute))
    return [storage]
  }
}

public struct ObservationIgnoredMacro: AccessorMacro {
  public static func expansion<
    Context: MacroExpansionContext,
    Declaration: DeclSyntaxProtocol
  >(
    of node: AttributeSyntax,
    providingAccessorsOf declaration: Declaration,
    in context: Context
  ) throws -> [AccessorDeclSyntax] {
    return []
  }
}

public struct ObservableValuesMacro { }

extension ObservableValuesMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let property = declaration.as(VariableDeclSyntax.self),
          property.isValidForValueObservation else {
      return []
    }

    if property.hasMacroApplication(ObservableMacro.ignoredMacroName) {
      return []
    }
    
    guard let identifier = property.identifier else {
      return []
    }
    
    guard let type = property.type else {
      throw DiagnosticsError(syntax: node, message: "@ObservableValues may only be applied to properties with specified types", id: .invalidApplication)
    }
    var modifiers = "\((property.visibility?.description ?? "")) "
    if property.isOverride {
      modifiers += "override "
    }
    if property.isComputed {
      let values: DeclSyntax =
        """
        \(raw: modifiers)var \(identifier)Values: \(raw: ObservableMacro.qualifiedValuesTypeName)<\(type)> {
          \(raw: ObservableMacro.qualifiedValuesTypeName)(of: self, computed: \\.\(identifier))
        }
        """
      return [values]
    } else {
      let storage = DeclSyntax(property.privatePrefixed("_", addingAttribute: ObservableMacro.ignoredAttribute, removingAttribute: ObservableMacro.valuesAttribute))
      let values: DeclSyntax =
        """
        \(raw: modifiers)var \(identifier)Values: \(raw: ObservableMacro.qualifiedValuesTypeName)<\(type)> {
          \(raw: ObservableMacro.qualifiedValuesTypeName)(of: self, stored: \\.\(identifier), registrar: \(raw: ObservableMacro.registrarVariableName))
        }
        """
      return [storage, values]
    }
  }
}
