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

/// Represents which witness we want to derive and how.
enum HashableRequirement {
  /// Derive the `hashValue` var using `_hashValue`
  case hashValue(isUnsafe: Bool)

  /// Derive the `hash(into:)` method using the deprecated way
  case compatHash(isUnsafe: Bool)

  /// Derive the `hash(into:)` method using the enum's raw value
  case hashRawValue(isUnsafe: Bool)

  /// Derive the `hash(into:)` method using the type's elements
  case hash(NominalTypeInfo)
}

/// Error type thrown by calling `HashableRequirement.fromSyntax` with ill-formed input
enum HashableRequirementParseError: Error {

  /// Expected \p node as an `FunctionCallExprSyntax` but got \p node instead
  case expectedFunctionCallLikeExpr(node: ExprSyntax)

  /// Expected `hash`, `hashValue`, `compatHash` or `hashRawValue` but got \p name
  case unknownRequirementName(name: String)
}

// Parsing utilities for HashableRequirement
extension HashableRequirement: TypeInfoProtocol {
  static func fromSyntax(node: ExprSyntax) throws -> Self {
    guard let fcall = node.as(FunctionCallExprSyntax.self) else {
      throw HashableRequirementParseError.expectedFunctionCallLikeExpr(node: node)
    }
    let requirementName = fcall.calledExpression.trimmedDescription
    switch requirementName {
    case "hash":
      let typeInfo = try fcall.arguments.expect(.init(parser: NominalTypeInfo.fromSyntax))
      return .hash(typeInfo)
    case "hashValue":
      return .hashValue(isUnsafe: try fcall.arguments.expect(.boolArg("isUnsafe")))
    case "compatHash":
      return .compatHash(isUnsafe: try fcall.arguments.expect(.boolArg("isUnsafe")))
    case "hashRawValue":
      return .hashRawValue(isUnsafe: try fcall.arguments.expect(.boolArg("isUnsafe")))
    default:
      throw HashableRequirementParseError.unknownRequirementName(name: requirementName)
    }
  }

  var syntax: ExprSyntax {
    switch self {
    case .hashValue(let isUnsafe):
      """
      hashValue(isUnsafe: \(raw: isUnsafe))
      """
    case .compatHash(let isUnsafe):
      """
      compatHash(isUnsafe: \(raw: isUnsafe))
      """
    case .hashRawValue(let isUnsafe):
      """
      hashRawValue(isUnsafe: \(raw: isUnsafe))
      """
    case .hash(let info):
      """
      hash(\(info.syntax))
      """
    }
  }

  var isUnsafe: Bool {
    switch self {
    case .hashValue(let isUnsafe):
      isUnsafe
    case .compatHash(let isUnsafe):
      isUnsafe
    case .hashRawValue(let isUnsafe):
      isUnsafe
    case .hash(let info):
      info.isUnsafe
    }
  }
}

public struct DeriveHashableMacro: DeclarationMacro {

  let requirement: HashableRequirement

  var unsafeMark: String {
    requirement.isUnsafe ? "unsafe " : ""
  }

  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    // Parse the requirement and then expand it.
    let req = try node.arguments.expect(
      .init(parser: HashableRequirement.fromStringLit))
    return [Self(requirement: req).expand]
  }

  /// Returns the expansion for `hashValue` var using `_hashValue`. When the
  /// conformance is unsafe, the call is combined through an `unsafe` expression.
  var expandHashValue: DeclSyntax {
    """
    var hashValue: Swift::Int {
      return \(raw: unsafeMark)Swift::_hashValue(for: self)
    }
    """
  }

  /// Returns the signature of the `hash(into:)` method.
  static var getHashSignature: DeclSyntax {
    """
    func hash(into hasher: inout Swift::Hasher)
    """
  }

  /// Returns the expansion of the `hash(into:)` method using the `hashValue` var.
  var expandCompatHash: DeclSyntax {
    """
    \(Self.getHashSignature) {
      \(raw: unsafeMark)hasher.combine(self.hashValue)
    }
    """
  }

  /// Returns the expansion of the `hash(into:)` method using the enum's raw value.
  var expandHashRawValue: DeclSyntax {
    """
    \(Self.getHashSignature) {
      \(raw: unsafeMark)hasher.combine(self.rawValue)
    }
    """
  }

  /// Derives the body of `hash(into:)` for an enum with no associated values
  static func getHashBodyNoAssociatedValues(_ infos: EnumTypeInfo) -> CodeBlockItemListSyntax {
    if infos.cases.isEmpty {
      return [
        """
        switch self {}
        """
      ]
    }
    var items = getDiscriminant(infos, scrutinee: "self", discrName: "discriminator")
    items += [
      """
      hasher.combine(discriminator)
      """
    ]
    return items
  }

  /// Derives the body of `hash(into:)` for an enum with associated values. When
  /// the conformance is unsafe, each associated value is combined through an
  /// `unsafe` expression, since it relies on the value's Hashable conformance.
  func getHashBodyHasAssociatedValues(_ infos: EnumTypeInfo) -> CodeBlockItemListSyntax {
    var idx = 0
    let cases: [SwitchCaseSyntax] =
      infos.cases.map { caseInfo in
        let fstStmt: [CodeBlockItemSyntax]
        /// Combine the element's index
        if caseInfo.isReachable {
          fstStmt = [
            """
            hasher.combine(\(raw: idx))
            """
          ]
          idx += 1
        } else {
          fstStmt = []
        }

        /// Combine each associated value in order
        let stmtsInCase: [CodeBlockItemSyntax] =
          if caseInfo.isReachable {
            (0..<caseInfo.associatedValueLabels.count).map { i in
              """
              \(raw: unsafeMark)hasher.combine(a\(raw: i))
              """
            }
          } else {
            [getUnreachableStatement()]
          }
        let pat = getEnumElementPayloadPattern(caseInfo, varPrefix: "a")
        return
          """
          case \(pat): 
            \(CodeBlockItemListSyntax(fstStmt + stmtsInCase))
          """
      }
    return
      """
      switch self {
      \(raw: cases.map { $0.trimmedDescription }.joined(separator: "\n"))
      }
      """
  }

  /// Derives the body of `hash(into:)` for an enum
  func getHashBody(_ infos: EnumTypeInfo) -> CodeBlockItemListSyntax {
    if infos.hasNoAssociatedValues() {
      Self.getHashBodyNoAssociatedValues(infos)
    } else {
      getHashBodyHasAssociatedValues(infos)
    }
  }

  /// Derives the body of `hash(into:)` for a struct. When the conformance is
  /// unsafe, each property is combined through an `unsafe` expression, since it
  /// relies on the property's Hashable conformance.
  func getHashBody(_ infos: StructTypeInfo) -> CodeBlockItemListSyntax {
    var items: [CodeBlockItemSyntax] = []
    for prop in infos.properties {
      if !prop.isUserAccessible {
        continue
      }
      items.append(
        """
        \(raw: unsafeMark)hasher.combine(self.\(raw: prop.name))
        """
      )
    }
    return .init(items)
  }

  /// Derives the body of `hash(into:)`
  func getHashBody(_ infos: NominalTypeInfo) -> CodeBlockItemListSyntax {
    switch infos.kind {
    case .enumLike(let enum_infos):
      return getHashBody(enum_infos)
    case .structLike(let struct_infos):
      return getHashBody(struct_infos)
    }
  }

  /// Derives the `hash(into:)` method
  func expandHash(_ infos: NominalTypeInfo) -> DeclSyntax {
    """
    \(Self.getHashSignature) {
      \(getHashBody(infos))
    }
    """
  }

  /// Derives a requirement for `Hashable`
  var expand: DeclSyntax {
    switch requirement {
    case .hashValue: expandHashValue
    case .compatHash: expandCompatHash
    case .hashRawValue: expandHashRawValue
    case .hash(let infos): expandHash(infos)
    }
  }
}
