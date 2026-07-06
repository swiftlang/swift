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
    return
      """
      \(getAttributes())
      static func \(getFunctionName())(_ a: Self, _ b: Self) -> Bool {
        \(getBody())
      }
      """
  }

  /// Attributes attached to the generated function. if the module is resilient, just a
  /// plain `==`. non-resilient types get the attributes that let the compiler treat it
  /// as the derived conformance witness.
  func getAttributes() -> AttributeListSyntax {
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
      \(raw: semantics)
      @_implements(Equatable, ==(_:_:))
      """
  }

  /// Name of the generated function: plain `==` when in a resilient module, otherwise a
  /// derived name.
  func getFunctionName() -> TokenSyntax {
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
  func getBody() -> CodeBlockItemListSyntax {
    switch info.kind {
    case .enumLike(let enumInfo) where enumInfo.isUninhabited():
      Self.getUninhabitedBody()
    case .enumLike(let enumInfo):
      Self.getEnumBody(enumInfo)
    case .structLike(let structInfo):
      Self.getStructBody(structInfo)
    }
  }

  /// `a == b` for a struct: compare stored, non-static properties in order,
  /// short-circuiting on the first mismatch.
  static func getStructBody(_ structInfo: StructTypeInfo) -> CodeBlockItemListSyntax {
    let comparedProperties = structInfo.properties.filter { !$0.isStatic }

    let guards: [CodeBlockItemSyntax] = comparedProperties.map { property in
      """
      guard a.\(raw: property.name) == b.\(raw: property.name) else {
        return false
      }
      """
    }

    return .init(guards + ["return true"])
  }

  /// `a == b` for an enum, picking the cheapest valid strategy for its shape.
  static func getEnumBody(
    _ enumInfo: EnumTypeInfo
  ) -> CodeBlockItemListSyntax {
    if enumInfo.hasNoAssociatedValues() {
      return getNoAssociatedValuesBody(enumInfo)
    }
    return getHasAssociatedValuesBody(enumInfo)
  }

  /// Body for an uninhabited enum: there are no cases to compare.
  static func getUninhabitedBody() -> CodeBlockItemListSyntax {
    """
    """
  }

  /// `a == b` for an enum with no associated values: compare discriminants.
  static func getNoAssociatedValuesBody(
    _ enumInfo: EnumTypeInfo
  ) -> CodeBlockItemListSyntax {
    var items = getDiscriminant(enumInfo, scrutinee: "a", discrName: "index_a")
    items += getDiscriminant(enumInfo, scrutinee: "b", discrName: "index_b")
    items += ["return index_a == index_b"]
    return items
  }

  /// `a == b` for an enum with associated values: match `(a, b)` against
  /// each case pairwise and compare bound payloads.
  static func getHasAssociatedValuesBody(
    _ enumInfo: EnumTypeInfo
  ) -> CodeBlockItemListSyntax {
    var cases: [SwitchCaseSyntax] = []
    for caseInfo in enumInfo.cases {
      var stmtsInCase: [CodeBlockItemSyntax] = []

      if caseInfo.isReachable {
        for i in 0..<caseInfo.associatedValueLabels.count {
          stmtsInCase.append(
            """
            guard l\(raw: i) == r\(raw: i) else {
              return false
            }
            """
          )
        }
        stmtsInCase.append("return true")
      } else {
        stmtsInCase.append(getUnreachableStatement())
      }

      let lPat = getEnumElementPayloadPattern(caseInfo, varPrefix: "l")
      let rPat = getEnumElementPayloadPattern(caseInfo, varPrefix: "r")

      cases.append(
        """
        case (\(lPat), \(rPat)): 
          \(CodeBlockItemListSyntax(stmtsInCase))
        """
      )
    }

    // A single-case enum's `(a, b)` switch is already exhaustive without a
    // default. Adding one for multi-case enums avoids an exhaustiveness
    // diagnostic for mismatched-case pairs (e.g. `(.foo, .bar)`).
    if enumInfo.cases.count > 1 {
      cases.append(
        """
        default: return false
        """
      )
    }

    return
      """
      switch (a, b) {
      \(raw: cases.map { $0.trimmedDescription }.joined(separator: "\n"))
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
) -> CodeBlockItemListSyntax {
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
          \(getUnreachableStatement())
        """
      )
    }
  }

  return
    """
    var \(raw: discrName): Int
    switch \(raw: scrutinee) {
    \(raw: cases.joined(separator: "\n"))
    }
    """
}

/// Pattern matching one enum case, optionally binding its associated
/// values with the given variable prefix. Unreachable cases bind nothing,
/// since their payload is never inspected.
func getEnumElementPayloadPattern(
  _ caseInfo: EnumCaseInfo,
  varPrefix: String
) -> PatternSyntax {
  if caseInfo.associatedValueLabels.isEmpty || !caseInfo.isReachable {
    return ".\(raw: caseInfo.name)"
  }

  let vars: [String] = caseInfo.associatedValueLabels.enumerated().map { i, name in
    let prefix = name.map { "\($0): " } ?? ""
    return "\(prefix)let \(varPrefix)\(i)"
  }

  return ".\(raw: caseInfo.name)(\(raw: vars.joined(separator: ", ")))"
}

/// A trap used for cases statically known to be unreachable at this call
/// site (e.g. pruned by availability).
func getUnreachableStatement() -> CodeBlockItemSyntax {
  """
  fatalError("Unavailable code reached")
  """
}
