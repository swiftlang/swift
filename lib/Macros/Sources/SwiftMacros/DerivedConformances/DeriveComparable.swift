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

// Note: this implementation is intentionnaly very close to `Equatable`'s derivation
// They will eventually be merged together.

/// Macro that derives an `<` implementation for an enum, given
/// type information produced by the compiler.
public struct DeriveComparableMacro: DeclarationMacro {

  let info: EnumTypeInfo
  let isResilient: Bool

  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let (typeInfo, isResilient) = try node.arguments.expect(
      .init(name: nil, parser: EnumTypeInfo.fromStringLit),
      .boolArg("isResilient")
    )

    return [
      Self(info: typeInfo, isResilient: isResilient).deriveComparable()
    ]
  }

  /// Builds the static `<` (or `__derived_enum_less_than`) function declaration.
  func deriveComparable() -> DeclSyntax {
    return
      """
      \(raw: getAttributes())
      static func \(raw: getFunctionName())(_ lhs: Self, _ rhs: Self) -> Bool {
        \(raw: getBody())
      }
      """
  }

  /// Attributes attached to the generated function. if the module is resilient, just a
  /// plain `<` non-resilient types get the attributes that let the compiler treat it
  /// as the derived conformance witness.
  func getAttributes() -> String {
    if isResilient {
      ""
    } else {
      """
      @_implements(Comparable, <(_:_:))
      """
    }
  }

  /// Name of the generated function: plain `<` when in a resilient module, otherwise a
  /// derived name.
  func getFunctionName() -> String {
    if isResilient {
      "<"
    } else {
      "__derived_enum_less_than"
    }
  }

  /// Dispatches to the right body builder depending on the type's shape.
  func getBody() -> String {
    if info.isUninhabited() {
      return Self.uninhabitedBody
    }
    if info.hasNoAssociatedValues {
      return noAssociatedValuesBody
    }
    return getHasAssociatedValuesBody
  }

  /// Body for an uninhabited enum: there are no cases to compare.
  static var uninhabitedBody: String {
    """
    """
  }

  /// `lhs < rhs` for an enum with no associated values: compare discriminants.
  var noAssociatedValuesBody: String {
    """
    \(getDiscriminant(info, scrutinee: "lhs", discrName: "index_lhs"))
    \(getDiscriminant(info, scrutinee: "rhs", discrName: "index_rhs"))
    return index_lhs < index_rhs
    """
  }

  /// `lhs < rhs` for an enum with associated values: match `(lhs, rhs)` against
  /// each case pairwise and compare bound payloads.
  var getHasAssociatedValuesBody: String {
    var cases: [String] = []
    for caseInfo in info.cases {
      let stmtsInCase: String

      if caseInfo.isReachable {
        let guards = (0..<caseInfo.associatedValueLabels.count).map {
          i in
          """
          guard l\(i) == r\(i) else {
            return l\(i) < r\(i)
          }
          """
        }
        stmtsInCase =
          """
          \(guards.joined(separator: "\n"))
          return false
          """
      } else {
        stmtsInCase = getUnreachableStatement()
      }

      let lPat = getEnumElementPayloadPattern(caseInfo, varPrefix: "l")
      let rPat = getEnumElementPayloadPattern(caseInfo, varPrefix: "r")

      cases.append(
        """
        case (\(lPat), \(rPat)): 
          \(stmtsInCase)
        """
      )
    }

    // A single-case enum's `(lhs, rhs)` switch is already exhaustive without a
    // default. Adding one for multi-case enums avoids an exhaustiveness
    // diagnostic for mismatched-case pairs (e.g. `(.foo, .bar)`).
    // This is unreachable as the case where the discriminants were different
    // is handled before.
    if info.cases.count > 1 {
      cases.append(
        """
        default: \(getUnreachableStatement())
        """
      )
    }

    return
      """
      \(getDiscriminant(info, scrutinee: "lhs", discrName: "index_lhs"))
      \(getDiscriminant(info, scrutinee: "rhs", discrName: "index_rhs"))
        if index_lhs != index_rhs {
          return index_lhs  < index_rhs
        }
        switch (lhs, rhs) {
        \(cases.joined(separator: "\n"))
        }
      """
  }
}
