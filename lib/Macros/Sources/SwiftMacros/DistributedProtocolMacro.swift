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

import SwiftSyntax
import SwiftSyntaxMacros
import SwiftDiagnostics
import SwiftOperators
import SwiftSyntaxBuilder

/// Introduces:
/// - `distributed actor $MyDistributedActor<ActorSystem>: $MyDistributedActor, _DistributedActorStub where ...`
/// - `extension MyDistributedActor where Self: _DistributedActorStub {}`
public struct DistributedProtocolMacro: ExtensionMacro, PeerMacro {
}

// ===== -----------------------------------------------------------------------
// MARK: Default Stub implementations Extension

extension DistributedProtocolMacro {

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

    let accessModifiers: String = proto.accessModifiersString

    let requirementStubs =
      proto.memberBlock.members // requirements
        .map { member in
          stubMethodDecl(access: accessModifiers, member.trimmed)
        }
        .joined(separator: "\n    ")

    let extensionDecl: DeclSyntax =
      """
      extension \(proto.name.trimmed) where Self: Distributed._DistributedActorStub {
        \(raw: requirementStubs)
      }
      """
    return [extensionDecl.cast(ExtensionDeclSyntax.self)]
  }

  static func stubMethodDecl(access: String, _ requirement: MemberBlockItemListSyntax.Element) -> String {
    // do we need to stub a computed variable?
    if let variable = requirement.decl.as(VariableDeclSyntax.self) {
      // TODO(distributed): improve stubbing computed properties of all kinds
      let name = variable.bindings.first!.pattern.trimmed
      let typeAnnotation = variable.bindings.first?.typeAnnotation.map { "\($0.trimmed)" } ?? "Any"
      return """
             \(access)\(variable.modifiers)\(variable.bindingSpecifier) \(name) \(typeAnnotation) {
               \(stubFunctionBody())
             }
             """
    }

    // normal function stub
    return """
           \(access)\(requirement) {
             \(stubFunctionBody())
           }
           """
  }

  static func stubFunctionBody() -> DeclSyntax {
    """
    if #available(SwiftStdlib 6.0, *) {
      Distributed._distributedStubFatalError()
    } else {
      fatalError()
    }
    """
  }
}

// ===== -----------------------------------------------------------------------
// MARK: Distributed Actor Stub type

extension DistributedProtocolMacro {

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

    var isGenericStub = false
    var specificActorSystemRequirement: TypeSyntax?

    if proto.genericWhereClause == nil {
      throw DiagnosticsError(
        syntax: node,
        message: """
                 Distributed protocol must declare actor system with SerializationRequirement, for example:
                    protocol Greeter<ActorSystem>: DistributedActor where ActorSystem: DistributedActorSystem<any Codable>
                 """, id: .invalidApplication)
    }

    let accessModifiers = proto.accessModifiersString

    for req in proto.genericWhereClause?.requirements ?? [] {
      switch req.requirement {
      case .conformanceRequirement(let conformanceReq)
           where conformanceReq.leftType.isActorSystem:
        specificActorSystemRequirement = conformanceReq.rightType.trimmed
        isGenericStub = true

      case .sameTypeRequirement(let sameTypeReq)
           where sameTypeReq.leftType.isActorSystem:
        specificActorSystemRequirement = sameTypeReq.rightType.trimmed
        isGenericStub = false

      default:
        continue
      }
    }

    if isGenericStub, let specificActorSystemRequirement {
      return [
        """
        \(proto.modifiers) distributed actor $\(proto.name.trimmed)<ActorSystem>: \(proto.name.trimmed), 
          Distributed._DistributedActorStub
          where ActorSystem: \(specificActorSystemRequirement) 
        { }
        """
      ]
    } else if let specificActorSystemRequirement {
      return [
        """
        \(proto.modifiers) distributed actor $\(proto.name.trimmed): \(proto.name.trimmed), 
          Distributed._DistributedActorStub
        { 
          \(typealiasActorSystem(access: accessModifiers, proto, specificActorSystemRequirement)) 
        }
        """
      ]
    } else {
      // there may be no `where` clause specifying an actor system,
      // but perhaps there is a typealias (or extension with a typealias),
      // specifying a concrete actor system so we let this synthesize
      // an empty `$Greeter` -- this may fail, or succeed depending on
      // surrounding code using a default distributed actor system,
      // or extensions providing it.
      return [
        """
        \(proto.modifiers) distributed actor $\(proto.name.trimmed): \(proto.name.trimmed), 
          Distributed._DistributedActorStub
        {
        }
        """
      ]
    }
  }

  private static func typealiasActorSystem(access: String, _ proto: ProtocolDeclSyntax, _ type: TypeSyntax) -> DeclSyntax {
    "\(raw: access)typealias ActorSystem = \(type)"
  }
}

// ===== -----------------------------------------------------------------------
// MARK: Convenience Extensions

extension ProtocolDeclSyntax {
  var accessModifiersString: String {
    let modifiers = modifiers.filter { modifier in
        modifier.isAccessControl
      }

    guard !modifiers.isEmpty else {
      return ""
    }

    let string = modifiers
      .map { "\($0.trimmed)" }
      .joined(separator: " ")
    return "\(string) "
  }
}

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

// ===== -----------------------------------------------------------------------
// MARK: DistributedProtocol macro errors

extension DistributedProtocolMacro {
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
      message: "'@DistributedProtocol' can only be applied to 'protocol', but was attached to '\(kind)'", id: .invalidApplication)
  }
}

struct DistributedProtocolMacroDiagnostic: DiagnosticMessage {
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
  init<S: SyntaxProtocol>(
    syntax: S,
    message: String,
    domain: String = "Distributed",
    id: DistributedProtocolMacroDiagnostic.ID,
    severity: SwiftDiagnostics.DiagnosticSeverity = .error) {
    self.init(diagnostics: [
      Diagnostic(
        node: Syntax(syntax),
        message: DistributedProtocolMacroDiagnostic(
          message: message,
          domain: domain,
          id: id,
          severity: severity))
    ])
  }
}
