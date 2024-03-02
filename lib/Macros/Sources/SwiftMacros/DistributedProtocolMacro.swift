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
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingExtensionsOf type: some TypeSyntaxProtocol,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [ExtensionDeclSyntax] {
    guard let proto = declaration.as(ProtocolDeclSyntax.self) else {
      return []
    }

    let requirements =
      proto.memberBlock.members.map { member in
        member.trimmed
      }
    let requirementStubs = requirements
      .map { req in
        """
        \(req) {
            if #available(SwiftStdlib 6.0, *) {
              Distributed._distributedStubFatalError()
            } else {
              fatalError()
            }
        }
        """
      }.joined(separator: "\n    ")

    let extensionDecl: DeclSyntax =
      """
      extension \(proto.name.trimmed) where Self: Distributed._DistributedActorStub {
        \(raw: requirementStubs)
      }
      """
    return [extensionDecl.cast(ExtensionDeclSyntax.self)]
  }

  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let proto = declaration.as(ProtocolDeclSyntax.self) else {
      return []
    }

    // FIXME must detect this off the protocol
    let serializationRequirementType =
      "Codable"

    let stubActorDecl: DeclSyntax =
      """
      distributed actor $\(proto.name.trimmed)<ActorSystem>: \(proto.name.trimmed), 
        Distributed._DistributedActorStub
        where ActorSystem: DistributedActorSystem<any \(raw: serializationRequirementType)>, 
          ActorSystem.ActorID: \(raw: serializationRequirementType) 
      { }
      """

    // return [extensionDecl, stubActorDecl]
    return [stubActorDecl]
  }

}
