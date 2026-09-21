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

/// Macro that derives an `==` implementation for a struct or enum, given
/// type information produced by the compiler.
public struct DeriveEquatableMacro: DeclarationMacro {

  let info: NominalTypeInfo
  let isResilient: Bool

  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let (typeInfo, isResilient) = try node.arguments.expect(
      .init(name: nil, parser: NominalTypeInfo.fromStringLit),
      .boolArg("isResilient")
    )

    return [
      Self(info: typeInfo, isResilient: isResilient).deriveEquatable()
    ]
  }

  /// Builds the static `==` (or `__derived_*_equals`) function declaration.
  func deriveEquatable() -> DeclSyntax {
    // A parameter of noncopyable type must state its ownership. Both operands
    // are only read, so borrow them.
    let ownership = info.isNoncopyable ? "borrowing " : ""
    return """
      \(raw: getAttributes())
      static func \(raw: getFunctionName())(_ lhs: \(raw: ownership)Self, _ rhs: \(raw: ownership)Self) -> Swift::Bool {
        \(raw: getBody())
      }
      """
  }

  /// Attributes attached to the generated function. if the module is resilient, just a
  /// plain `==`. non-resilient types get the attributes that let the compiler treat it
  /// as the derived conformance witness.
  func getAttributes() -> String {
    if isResilient {
      return ""
    }
    let semantics: String
    switch info.kind {
    case .enumLike:
      semantics = "@_semantics(\"derived_enum_equals\")"
    case .structLike:
      semantics = ""
    }
    return
      """
      \(semantics)
      @_implements(Swift::Equatable, ==(_:_:))
      """
  }

  /// Name of the generated function: plain `==` when in a resilient module, otherwise a
  /// derived name.
  func getFunctionName() -> String {
    if isResilient {
      return "=="
    }
    switch info.kind {
    case .enumLike:
      return "__derived_enum_equals"
    case .structLike:
      return "__derived_struct_equals"
    }
  }

  /// Dispatches to the right body builder depending on the type's shape.
  func getBody() -> String {
    switch info.kind {
    // An uninhabited enum has no cases to compare.
    case .enumLike(let enumInfo) where enumInfo.isUninhabited():
      ""
    case .enumLike(let enumInfo):
      Self.getEnumBody(enumInfo)
    case .structLike(let structInfo):
      Self.getStructBody(structInfo)
    }
  }

  /// `a == b` for a struct: compare stored, non-static properties in order,
  /// short-circuiting on the first mismatch.
  static func getStructBody(_ structInfo: StructTypeInfo) -> String {
    let guards = structInfo.properties.filter { !$0.isStatic }.map { property in
      """
      guard lhs.\(property.name) == rhs.\(property.name) else {
        return false
      }
      """
    }

    return (guards + ["return true"]).joined(separator: "\n")
  }

  /// `a == b` for an enum, picking the cheapest valid strategy for its shape.
  static func getEnumBody(
    _ enumInfo: EnumTypeInfo
  ) -> String {
    if enumInfo.hasNoAssociatedValues() {
      getNoAssociatedValuesBody(enumInfo)
    } else {
      getHasAssociatedValuesBody(enumInfo)
    }
  }

  /// `a == b` for an enum with no associated values: compare discriminants.
  static func getNoAssociatedValuesBody(
    _ enumInfo: EnumTypeInfo
  ) -> String {
    """
    \(getDiscriminant(enumInfo, scrutinee: "lhs", discrName: "index_lhs"))
    \(getDiscriminant(enumInfo, scrutinee: "rhs", discrName: "index_rhs"))
    return index_lhs == index_rhs
    """
  }

  /// `a == b` for an enum with associated values: match `(a, b)` against
  /// each case pairwise and compare bound payloads.
  static func getHasAssociatedValuesBody(
    _ enumInfo: EnumTypeInfo
  ) -> String {
    var cases = enumInfo.cases.map { caseInfo in
      let stmtsInCase: [String]
      if caseInfo.isReachable {
        stmtsInCase =
          (0..<caseInfo.associatedValueLabels.count).map { i in
            """
            guard l\(i) == r\(i) else {
              return false
            }
            """
          } + ["return true"]
      } else {
        stmtsInCase = [unreachableStatement]
      }

      let lPat = getEnumElementPayloadPattern(caseInfo, varPrefix: "l")
      let rPat = getEnumElementPayloadPattern(caseInfo, varPrefix: "r")

      return """
        case (\(lPat), \(rPat)):
          \(stmtsInCase.joined(separator: "\n"))
        """
    }

    // A single-case enum's `(a, b)` switch is already exhaustive without a
    // default. Adding one for multi-case enums avoids an exhaustiveness
    // diagnostic for mismatched-case pairs (e.g. `(.foo, .bar)`).
    if enumInfo.cases.count > 1 {
      cases.append("default: return false")
    }

    return
      """
      switch (lhs, rhs) {
      \(cases.joined(separator: "\n"))
      }
      """
  }
}

extension EnumTypeInfo {
  /// True if no case in this enum carries associated values.
  func hasNoAssociatedValues() -> Bool {
    cases.allSatisfy(\.associatedValueLabels.isEmpty)
  }

  /// True if this enum has no cases at all.
  func isUninhabited() -> Bool {
    cases.isEmpty
  }
}

/// Builds a `switch` over `scrutinee` that assigns a dense `Int`
/// discriminant (skipping unreachable cases) into `discrName`.
func getDiscriminant(
  _ enumInfo: EnumTypeInfo,
  scrutinee: String,
  discrName: String
) -> String {
  var nextDiscriminant = 0
  var cases: [String] = []
  for caseInfo in enumInfo.cases {
    if caseInfo.isReachable {
      cases.append(
        """
        case .\(caseInfo.name):
          \(discrName) = \(nextDiscriminant)
        """
      )
      nextDiscriminant += 1
    } else {
      cases.append(
        """
        case .\(caseInfo.name):
          \(unreachableStatement)
        """
      )
    }
  }

  return
    """
    var \(discrName): Swift::Int
    switch \(scrutinee) {
    \(cases.joined(separator: "\n"))
    }
    """
}

/// Pattern matching one enum case, optionally binding its associated
/// values with the given variable prefix. Unreachable cases bind nothing,
/// since their payload is never inspected.
func getEnumElementPayloadPattern(
  _ caseInfo: EnumCaseInfo,
  varPrefix: String
) -> String {
  if caseInfo.associatedValueLabels.isEmpty || !caseInfo.isReachable {
    return ".\(caseInfo.name)"
  }

  let vars: [String] = caseInfo.associatedValueLabels.enumerated().map { i, name in
    let prefix = name.map { "\($0): " } ?? ""
    return "\(prefix)let \(varPrefix)\(i)"
  }

  return ".\(caseInfo.name)(\(vars.joined(separator: ", ")))"
}

/// A trap used for cases statically known to be unreachable at this call
/// site (e.g. pruned by availability).
let unreachableStatement: String =
  """
  Swift::fatalError("Unavailable code reached")
  """
