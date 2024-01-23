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

extension DistributedMakeProtocolStubTypeMacro {
  private static let StubsMacroNamePrefix = "_distributed_stubs_"

  private enum MacroNameKind {
    case hash
    case decl
    case none
  }

  private static func _distributed_stubs(_ hash: MacroNameKind = .none, _ name: String) -> String {
    if hash == .hash {
      "#\(Self.StubsMacroNamePrefix)\(name)"
    } else {
      "\(Self.StubsMacroNamePrefix)\(name)\(hash == .decl ? "()" : "")"
    }
  }
  private static func _distributed_stubs(_ hash: MacroNameKind = .none, _ decl: ProtocolDeclSyntax) -> String {
    _distributed_stubs(hash, "\(decl.name.trimmed)")
  }
}


/// Introduces:
/// - `distributed actor $MyDistributedActor<ActorSystem>`
/// - `macro _distributed_stubs_Greeter()`
public struct DistributedMakeProtocolStubTypeMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let proto = declaration.as(ProtocolDeclSyntax.self) else {
      return []
    }

    let selfStubsMacro: DeclSyntax = "#\(raw: _distributed_stubs(.hash, proto))"

    // --- requirements
    let requirements =
      proto.memberBlock.members.map { member in
        member.trimmed
      }
    let requirementsAsStringParams = requirements
      .map { "\"\($0.trimmed)\"" }
      .joined(separator: ",\n    ")

    // --- protocols to stub
    var inheritedProtocols: [TypeSyntax] = []
    if let inheritanceClause = proto.inheritanceClause {
      for inheritedType in inheritanceClause.inheritedTypes {
        guard "\(inheritedType.type.trimmed)" != "DistributedActor" else {
          continue
        }

        inheritedProtocols.append(inheritedType.type)
      }
    }
    let protocolsAsStringParams = inheritedProtocols
      .map { "\"\($0.trimmed)\"" }
      .joined(separator: ",\n    ")

    // --- introducedNames
    let introducedNames: [String] =
      if inheritedProtocols.isEmpty {
        requirements.compactMap { $0.memberIdentifier }
      } else {
        []
      }

    let introducedNamesString: String
    if inheritedProtocols.isEmpty, introducedNames.isEmpty {
      introducedNamesString = ""
    } else if inheritedProtocols.isEmpty {
      let names = introducedNames
        .map { named("\($0)") }
        .joined(separator: ", ")
      introducedNamesString = ", names: \(names)"
    } else {
      introducedNamesString = ", names: arbitrary"
    }

    let stubInheritanceClauseString: String
    if inheritedProtocols.isEmpty {
      stubInheritanceClauseString = "\(proto.name)"
    } else {
      let protocolsAsInheritanceClauseList = inheritedProtocols
        .map { "\($0.trimmed)" }
        .joined(separator: ", ")
      stubInheritanceClauseString = "\(proto.name), \(protocolsAsInheritanceClauseList)"
    }

    let serializationRequirementWhereClause: String =
      "where SerializationRequirement == any Codable" // FIXME: this is hardcoded

    let commaIfHasRequirements = !requirements.isEmpty ? "," : ""

    let stubsMacroBody: DeclSyntax =
      """
      #distributedStubs(
          module: "main", protocolName: "\(proto.name.trimmed)",
          stubProtocols: [\(raw: protocolsAsStringParams)]\(raw: commaIfHasRequirements)
          \(raw: requirementsAsStringParams)
        )
      """

    let inheritedProtocolStubMacros: String = inheritedProtocols
      .compactMap { proto in proto.identifier.map { ident in _distributed_stubs(.hash, ident) } }
      .joined(separator: "\n    ")

    let stubActorDecl: DeclSyntax =
      """
      // distributed actor $\(proto.name)<ActorSystem>: \(raw: stubInheritanceClauseString) \(raw: serializationRequirementWhereClause) {
      distributed actor $\(proto.name): \(raw: stubInheritanceClauseString) {
        typealias ActorSystem = LocalTestingDistributedActorSystem // FIXME: remove this

        // TODO: workaround since we cannot expand the macro; so we have to copy the body into here
        \(stubsMacroBody)

        // stub inherited members
        \(raw: inheritedProtocolStubMacros)
      }
      """

    let stubsMacroDecl: DeclSyntax =
      """
      @freestanding(declaration\(raw: introducedNamesString))
      macro \(raw: _distributed_stubs(.decl, proto)) =
        \(stubsMacroBody)
      """

    return [stubsMacroDecl, stubActorDecl]
  }

  private static func named(_ name: String) -> String {
    "named(\(name))"
  }

}