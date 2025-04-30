//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

@_spi(ExperimentalLanguageFeatures)
import SwiftSyntax
import SwiftSyntaxMacros
import SwiftDiagnostics
import SwiftSyntaxBuilder

/// Introduces:
/// - `distributed actor $MyDistributedActor<ActorSystem>: $MyDistributedActor, _DistributedActorStub where ...`
/// - `extension MyDistributedActor where Self: _DistributedActorStub {}`
public struct DistributedResolvableMacro: ExtensionMacro, PeerMacro {
}

// ===== -----------------------------------------------------------------------
// MARK: Default Stub implementations Extension

extension DistributedResolvableMacro {

  /// Introduce the `extension MyDistributedActor` which contains default
  /// implementations of the protocol's requirements.
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingExtensionsOf type: some TypeSyntaxProtocol,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [ExtensionDeclSyntax] {
    guard let proto = declaration.as(ProtocolDeclSyntax.self) else {
      // we diagnose here, only once
      try throwIllegalTargetDecl(node: node, declaration)
    }

    guard !proto.memberBlock.members.isEmpty else {
      // ok, the protocol has no requirements so we no-op it
      return []
    }

    let attributes = proto.attributes.filter { attr in
      attr.as(AttributeSyntax.self)?.attributeName.trimmed.description == "_spi"
    }
    let accessModifiers = proto.accessControlModifiers

    let requirementStubs =
      proto.memberBlock.members // requirements
        .filter { member in
          switch member.decl.kind {
          case .functionDecl: return true
          case .variableDecl: return true
          default:
            return false
          }
        }
        .map { member in
          stubMethodDecl(access: accessModifiers, member.trimmed)
        }
        .joined(separator: "\n    ")

    let extensionDecl: DeclSyntax =
      """
      \(raw: attributes.map({$0.description}).joined(separator: "\n"))
      extension \(proto.name.trimmed) where Self: Distributed._DistributedActorStub {
        \(raw: requirementStubs)
      }
      """
    return [extensionDecl.cast(ExtensionDeclSyntax.self)]
  }

  static func stubMethodDecl(access: DeclModifierListSyntax, _ requirement: MemberBlockItemListSyntax.Element) -> String {
    // do we need to stub a computed variable?
    if var variable = requirement.decl.as(VariableDeclSyntax.self) {
      variable.modifiers = variable.modifiers.filter { !$0.isAccessControl }
      access.reversed().forEach { modifier in
        variable.modifiers = [modifier] + variable.modifiers
      }

      var accessorStubs: [String] = []
      for binding in variable.bindings {
        if let accessorBlock = binding.accessorBlock {
          for accessor in accessorBlock.accessors.children(viewMode: .all) {
            let accessorStub = "\(accessor) { \(stubFunctionBody()) }"
            accessorStubs.append(accessorStub)
          }
        }
      }

      let name = variable.bindings.first!.pattern.trimmed
      let typeAnnotation = variable.bindings.first?.typeAnnotation.map { "\($0.trimmed)" } ?? "Any"

      // computed property stub
      return """
             \(variable.attributes)
             \(variable.modifiers)\(variable.bindingSpecifier) \(name) \(typeAnnotation) {
               \(accessorStubs.joined(separator: "\n  "))
             }
             """
    } else if var fun = requirement.decl.as(FunctionDeclSyntax.self) {
      fun.modifiers = fun.modifiers.filter { !$0.isAccessControl }
      access.reversed().forEach { modifier in
        fun.modifiers = [modifier] + fun.modifiers
      }

      // normal function stub
      return """
             \(fun) {
               \(stubFunctionBody())
             }
             """
    } else {
      // some declaration type we could not handle, let's silently ignore; we should not really need to emit
      // anything others than var and func here, and it's cleaner to just ignore rather than crash here.
      return ""
    }
  }

  static func stubFunctionBody() -> DeclSyntax {
    """
    if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
      Distributed._distributedStubFatalError()
    } else {
      fatalError()
    }
    """
  }
}

// ===== -----------------------------------------------------------------------
// MARK: Distributed Actor Stub type

extension DistributedResolvableMacro {

  /// Introduce the `distributed actor` stub type.
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let proto = declaration.as(ProtocolDeclSyntax.self) else {
      // don't diagnose here (again),
      // we'll already report an error here from the other macro role
      return []
    }

    var isGenericOverActorSystem = false
    var specificActorSystemRequirement: TypeSyntax?

    let attributes = proto.attributes.filter {
      guard let attr = $0.as(AttributeSyntax.self) else {
        return false
      }
      guard let ident = attr.attributeName.as(IdentifierTypeSyntax.self) else {
        return false
      }
      return ident.name.text == "_spi"
    }
    let accessModifiers = proto.accessControlModifiers

    for req in proto.genericWhereClause?.requirements ?? [] {
      switch req.requirement {
      case .conformanceRequirement(let conformanceReq)
           where conformanceReq.leftType.isActorSystem:
        specificActorSystemRequirement = conformanceReq.rightType.trimmed
        isGenericOverActorSystem = true

      case .sameTypeRequirement(let sameTypeReq):
        switch sameTypeReq.leftType {
        case .type(let type) where type.isActorSystem:
          switch sameTypeReq.rightType.trimmed {
          case .type(let rightType):
            specificActorSystemRequirement = rightType
            isGenericOverActorSystem = false

          case .expr:
            throw DiagnosticsError(
              syntax: sameTypeReq.rightType,
              message: "Expression type not supported for distributed actor",
              id: .invalidGenericArgument
            )
          }

        default:
          continue
        }

      default:
        continue
      }
    }

    var primaryAssociatedTypes: [PrimaryAssociatedTypeSyntax] = []
    if let primaryTypes = proto.primaryAssociatedTypeClause?.primaryAssociatedTypes {
      primaryAssociatedTypes.append(contentsOf: primaryTypes)
    }

    // The $Stub is always generic over the actor system: $Stub<ActorSystem>
    var primaryTypeParams: [String] = primaryAssociatedTypes.map {
      $0.name.trimmed.text
    }

    // Don't duplicate the ActorSystem type parameter if it already was declared
    // on the protocol as a primary associated type;
    // otherwise, add it as first primary associated type.
    let actorSystemTypeParam: [String]
    if primaryTypeParams.contains("ActorSystem") {
      actorSystemTypeParam = []
    } else if isGenericOverActorSystem {
      actorSystemTypeParam = ["ActorSystem"]
    } else {
      actorSystemTypeParam = []
    }

    // Prepend the actor system type parameter, as we want it to be the first one
    primaryTypeParams = actorSystemTypeParam + primaryTypeParams
    let typeParamsClause =
      primaryTypeParams.isEmpty ? "" : "<" + primaryTypeParams.joined(separator: ", ") + ">"

    var whereClause: String = ""
    do {
      let associatedTypeDecls = proto.associatedTypeDecls
      var typeParamConstraints: [String] = []
      for typeParamName in primaryTypeParams {
        if let decl = associatedTypeDecls[typeParamName] {
          if let inheritanceClause = decl.inheritanceClause {
            typeParamConstraints.append("\(typeParamName)\(inheritanceClause)")
          }
        }
      }

      if isGenericOverActorSystem, let specificActorSystemRequirement {
        typeParamConstraints = ["ActorSystem: \(specificActorSystemRequirement)"] + typeParamConstraints
      }
      
      if !typeParamConstraints.isEmpty {
        whereClause += "\n  where " + typeParamConstraints.joined(separator: ",\n  ")
      }
    }

    let stubActorBody: String
    if isGenericOverActorSystem {
      // there may be no `where` clause specifying an actor system,
      // but perhaps there is a typealias (or extension with a typealias),
      // specifying a concrete actor system so we let this synthesize
      // an empty `$Greeter` -- this may fail, or succeed depending on
      // surrounding code using a default distributed actor system,
      // or extensions providing it.
      stubActorBody = ""
    } else if let specificActorSystemRequirement {
      stubActorBody = "\(typealiasActorSystem(access: accessModifiers, proto, specificActorSystemRequirement))"
    } else {
      stubActorBody = ""
    }

    return [
      """
      \(attributes)
      \(proto.modifiers) distributed actor $\(proto.name.trimmed)\(raw: typeParamsClause): \(proto.name.trimmed), 
        Distributed._DistributedActorStub \(raw: whereClause)
      {
        \(raw: stubActorBody)
      }
      """
    ]
  }

  private static func typealiasActorSystem(access: DeclModifierListSyntax,
                                           _ proto: ProtocolDeclSyntax,
                                           _ type: TypeSyntax) -> DeclSyntax {
    "\(access)typealias ActorSystem = \(type)"
  }
}

// ===== -----------------------------------------------------------------------
// MARK: Convenience Extensions

extension TypeSyntax {
  fileprivate var isActorSystem: Bool {
    self.trimmedDescription == "ActorSystem"
  }
}

extension DeclSyntaxProtocol {
  var isClass: Bool {
    return self.is(ClassDeclSyntax.self)
  }

  var isActor: Bool {
    return self.is(ActorDeclSyntax.self)
  }

  var isEnum: Bool {
    return self.is(EnumDeclSyntax.self)
  }

  var isStruct: Bool {
    return self.is(StructDeclSyntax.self)
  }
}

extension DeclModifierSyntax {
  var isAccessControl: Bool {
    switch self.name.tokenKind {
    case .keyword(.private): fallthrough
    case .keyword(.fileprivate): fallthrough
    case .keyword(.internal): fallthrough
    case .keyword(.package): fallthrough
    case .keyword(.public):
      return true
    default:
      return false
    }
  }
}

extension ProtocolDeclSyntax {
  var associatedTypeDecls: [String: AssociatedTypeDeclSyntax] {
    let visitor = AssociatedTypeDeclVisitor(viewMode: .all)
    visitor.walk(self)
    return visitor.associatedTypeDecls
  }

  final class AssociatedTypeDeclVisitor: SyntaxVisitor {
    var associatedTypeDecls: [String: AssociatedTypeDeclSyntax] = [:]

    override func visit(_ node: AssociatedTypeDeclSyntax) -> SyntaxVisitorContinueKind {
      associatedTypeDecls[node.name.text] = node
      return .skipChildren
    }
  }
}

// ===== -----------------------------------------------------------------------
// MARK: @Distributed.Resolvable macro errors

extension DistributedResolvableMacro {
  static func throwIllegalTargetDecl(node: AttributeSyntax, _ declaration: some DeclSyntaxProtocol) throws -> Never {
    let kind: String
    if declaration.isClass {
      kind = "class"
    } else if declaration.isActor {
      kind = "actor"
    } else if declaration.isStruct {
      kind = "struct"
    } else if declaration.isStruct {
      kind = "enum"
    } else {
      kind = "\(declaration.kind)"
    }

    throw DiagnosticsError(
      syntax: node,
      message: "'@Resolvable' can only be applied to 'protocol', but was attached to '\(kind)'", id: .invalidApplication)
  }
}

struct DistributedResolvableMacroDiagnostic: DiagnosticMessage {
  enum ID: String {
    case invalidApplication = "invalid type"
    case missingInitializer = "missing initializer"
    case invalidGenericArgument = "invalid generic argument"
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
    domain: String = "Distributed",
    id: DistributedResolvableMacroDiagnostic.ID,
    severity: SwiftDiagnostics.DiagnosticSeverity = .error) {
    self.init(diagnostics: [
      Diagnostic(
        node: Syntax(syntax),
        message: DistributedResolvableMacroDiagnostic(
          message: message,
          domain: domain,
          id: id,
          severity: severity))
    ])
  }
}
