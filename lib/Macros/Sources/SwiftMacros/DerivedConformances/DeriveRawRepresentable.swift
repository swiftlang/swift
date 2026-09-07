//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

/// Witness to derive for the `RawRepresentable` protocol.
enum RawRepresentableWitness: TypeInfoProtocol {

  /// The `rawValue` var
  case varDef

  /// The `init(rawValue:)` initializer
  case initializer

  /// Parsing utility
  static func fromSyntax(node: ExprSyntax) throws -> RawRepresentableWitness {
    switch node.trimmedDescription {
    case "varDef": .varDef
    case "initializer": .initializer
    default:
      fatalError(
        "Expected `varDef` or `initializer` but got \(node.trimmedDescription)"
      )
    }
  }

  var syntax: ExprSyntax {
    switch self {
    case .varDef: "varDef"
    case .initializer: "initializer"
    }
  }
}

public struct DeriveRawRepresentableMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    // Expecting #_deriveRawRepresentable(<type info>, <witness>, <isStrictMemorySafety>)

    let (typeInfo, witnessKind, isStrictMemorySafety) = try node.arguments.expect(
      .init(parser: NominalTypeInfo.fromStringLit),
      .init(parser: RawRepresentableWitness.fromStringLit),
      .boolArg(nil)
    )
    let enumInfo: EnumTypeInfo

    switch typeInfo.kind {
    case .enumLike(let info):
      enumInfo = info
    default: fatalError("Expected an enum")
    }

    // Derive the appropriate witness for the `witnessKind` requirement
    let witness: DeclSyntax

    switch witnessKind {
    case .varDef:
      witness = expandVarDef(enumInfo, isStrictMemorySafety: isStrictMemorySafety)
    case .initializer:
      witness = expandInitializer(enumInfo)
    }

    return [witness]
  }

  /// Expands the `rawValue` var declaration
  static func expandVarDef(
    _ enumInfo: EnumTypeInfo, isStrictMemorySafety: Bool
  ) -> DeclSyntax {
    let rawType = enumInfo.rawTypeName!

    if enumInfo.isObjC {
      // ObjC enums are represented by their raw value, so just use a bitcast.
      let unsafePrefix = isStrictMemorySafety ? "unsafe " : ""
      return
        """
        nonisolated var rawValue: \(raw: rawType) {
          return \(raw: unsafePrefix)Swift::unsafeBitCast(self, to: \(raw: rawType).self)
        }
        """
    }

    let cases = enumInfo.cases.map { c in
      "case .\(c.name): return \(c.rawValue!)"
    }.joined(separator: "\n")

    return
      """
      nonisolated var rawValue: \(raw: rawType) {
        switch self {
        \(raw: cases)
        }
      }
      """
  }

  /// Expands the `init(rawValue:)` declaration
  static func expandInitializer(_ enumInfo: EnumTypeInfo) -> DeclSyntax {
    let rawType = enumInfo.rawTypeName!

    let cases: [String] = enumInfo.cases.compactMap { c in
      switch c.runtimeAvailability {
      case .unavailable:
        return nil
      case .always:
        return
          """
          case \(c.rawValue!):
            self = .\(c.name)
          """
      case .conditional(let platform, let version):
        return
          """
          case \(c.rawValue!):
            guard #available(\(platform) \(version), *) else { return nil }
            self = .\(c.name)
          """
      }
    }

    return
      """
      init?(rawValue: \(raw: rawType)) {
        switch rawValue {
        \(raw: cases.joined(separator: "\n"))
        default:
          return nil
        }
      }
      """
  }
}
