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

enum CodingKeyType: TypeInfoProtocol {
  case int
  case string

  static func fromSyntax(node: ExprSyntax) throws -> Self {
    switch node.trimmedDescription {
    case "int": return .int
    case "string": return .string
    default: fatalError("expected `int` or `string` but got `\(node.trimmedDescription)`")
    }
  }

  var syntax: ExprSyntax {
    "\(raw: self.prefix)"
  }

  var typeName: String {
    switch self {
    case .int: return "Swift::Int"
    case .string: return "Swift::String"
    }
  }

  var prefix: String {
    switch self {
    case .int: return "int"
    case .string: return "string"
    }
  }
}

enum CodingKeyWitness: TypeInfoProtocol {

  case `init`
  case valueVar

  static func fromSyntax(node: ExprSyntax) throws -> CodingKeyWitness {
    switch node.trimmedDescription {
    case "`init`", "init": return .`init`
    case "valueVar": return .valueVar
    default:
      fatalError(
        "Expected `init`, `valueVar` but got `\(node.trimmedDescription)`"
      )
    }
  }

  var syntax: ExprSyntax {
    switch self {
    case .`init`: return "init"
    case .valueVar: return "valueVar"
    }
  }
}

public struct DeriveCodingKeyMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let (typeInfo, witnessKind, rawType, usingRaw) = try node.arguments.expect(
      .init(parser: NominalTypeInfo.fromStringLit),
      .init(parser: CodingKeyWitness.fromStringLit),
      .init(parser: CodingKeyType.fromStringLit),
      .boolArg(nil)
    )
    let enumInfo: EnumTypeInfo

    switch typeInfo.kind {
    case .enumLike(let info):
      enumInfo = info
    default: fatalError("Expected an enum")
    }

    let witness: DeclSyntax

    switch witnessKind {
    case .`init`: witness = expandInit(enumInfo, rawType, usingRaw)
    case .valueVar: witness = expandValueVar(enumInfo, rawType, usingRaw)
    }

    return [witness]
  }

  static func expandInit(_ enumInfo: EnumTypeInfo, _ ty: CodingKeyType, _ usingRaw: Bool)
    -> DeclSyntax
  {
    if usingRaw {
      return
        """
        init?(\(raw: ty.prefix)Value: \(raw: ty.typeName)) {
          self.init(rawValue: \(raw: ty.prefix)Value)
        }
        """
    }

    switch ty {
    case .int:

      return
        """
        init?(intValue: Int) {
            return nil
        }
        """
    case .string:
      let cases = enumInfo.cases.compactMap { c -> String? in
        switch c.runtimeAvailability {
        case .unavailable:
          return nil
        case .always:
          return
            """
            case "\(c.name)": self = .\(c.name)
            """
        case .conditional(let platform, let version):
          return
            """
            case "\(c.name)":
              guard #available(\(platform) \(version), *) else { return nil }
              self = .\(c.name)
            """
        }
      }.joined(separator: "\n")
      return
        """
        init?(stringValue: Swift::String) {
          switch stringValue {
            \(raw: cases)
            default: return nil
          }
        }
        """
    }

  }

  static func expandValueVar(_ enumInfo: EnumTypeInfo, _ ty: CodingKeyType, _ usingRaw: Bool)
    -> DeclSyntax
  {
    let body: String
    let retType: String
    switch ty {
    case .int:
      retType = "\(ty.typeName)?"
    case .string:
      retType = ty.typeName
    }

    if usingRaw {
      body = "return self.rawValue"
    } else {
      switch ty {
      case .int:
        body = "return nil"
      case .string:
        let cases = enumInfo.cases.map { c in
          """
          case .\(c.name): return "\(c.name)"
          """
        }.joined(separator: "\n")

        if enumInfo.cases.isEmpty {
          body =
            """
            return ""
            """
        } else {
          body = """
            switch self {
              \(cases)
            }
            """
        }
      }
    }

    return
      """
      var \(raw: ty.prefix)Value: \(raw: retType) {
        \(raw: body)
      }
      """
  }
}
