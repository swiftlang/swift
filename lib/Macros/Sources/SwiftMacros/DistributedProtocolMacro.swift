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

    let requirements =
      proto.memberBlock.members.map { member in
        member.trimmed
      }
    let requirementStubs = requirements
      .map(stubMethod).joined(separator: "\n    ")

    let extensionDecl: DeclSyntax =
      """
      extension \(proto.name.trimmed) where Self: Distributed._DistributedActorStub {
        \(raw: requirementStubs)
      }
      """
    return [extensionDecl.cast(ExtensionDeclSyntax.self)]
  }

  static func stubMethod(_ requirementDeclaration: MemberBlockItemListSyntax.Element) -> String {
    """
    \(requirementDeclaration) {
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

    guard let genericWhereClause = proto.genericWhereClause else {
      guard !proto.memberBlock.members.isEmpty else {
        // ok, the protocol has no requirements so we no-op it
        return []
      }
      throw DiagnosticsError(
        syntax: node,
        message: """
                 Distributed protocol must declare actor system with SerializationRequirement, for example:
                    protocol Greeter<ActorSystem>: DistributedActor where ActorSystem: DistributedActorSystem<any Codable>
                 """, id: .invalidApplication)
    }

    for req in genericWhereClause.requirements {
      print("req.requirement: \(req.requirement)")
      switch req.requirement {
      case .conformanceRequirement(let conformanceReq)
           where conformanceReq.leftType.isActorSystem:
        print("conf: \(conformanceReq)")
        specificActorSystemRequirement = conformanceReq.rightType.trimmed
        isGenericStub = true

      case .sameTypeRequirement(let sameTypeReq)
           where sameTypeReq.leftType.isActorSystem:
        print("same type: \(sameTypeReq)")
        specificActorSystemRequirement = sameTypeReq.rightType.trimmed
        isGenericStub = false

      default:
        print("SKIP: \(req)")
        continue
      }
    }

    let stubActorDecl: DeclSyntax =
      if isGenericStub, let specificActorSystemRequirement {
        """
        \(proto.modifiers) distributed actor $\(proto.name.trimmed)<ActorSystem>: \(proto.name.trimmed), 
          Distributed._DistributedActorStub
          where ActorSystem: \(specificActorSystemRequirement) 
        { }
        """
      } else if let specificActorSystemRequirement {
        """
        \(proto.modifiers) distributed actor $\(proto.name.trimmed): \(proto.name.trimmed), 
          Distributed._DistributedActorStub
        { 
          \(typealiasActorSystem(proto, specificActorSystemRequirement)) 
        }
        """
      } else {
        throw DiagnosticsError(
          syntax: node,
          message: "'@DistributedProtocol' cannot be applied to ", id: .invalidApplication)
      }

    return [stubActorDecl]
  }

  private static func typealiasActorSystem(_ proto: ProtocolDeclSyntax, _ type: TypeSyntax) -> DeclSyntax {
    "typealias ActorSystem = \(type)"
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

// ===== -----------------------------------------------------------------------
// MARK: DistributedProtocol macro errors

extension DistributedProtocolMacro {
  static func throwIllegalTargetDecl(node: AttributeSyntax, _ declaration: some DeclSyntaxProtocol) throws -> Never {
    let kind =
      if declaration.isClass {
        "class"
      } else if declaration.isActor {
        "actor"
      } else if declaration.isStruct {
        "struct"
      } else if declaration.isStruct {
        "enum"
      } else {
        "\(declaration.kind)"
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
