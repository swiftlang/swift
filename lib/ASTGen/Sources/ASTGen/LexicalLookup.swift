//===--- LexicalLookup.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftIfConfig
@_spi(Experimental) import SwiftLexicalLookup
import SwiftSyntax

private let rowCharWidth: Int = 30

/// This function validates output of SwiftLexicalLookup
/// against the output of `ASTScope` passed to `astScopeResultRef`.
///
/// The function assigns specific flags to obtained names. The flags are applied in three phases:
/// - `ASTScope` name extraction - The names obtained from `astScopeResultRef` are
///   mapped to `ConsumedLookupResult` common representation. The names receive flags
///   independently from `SwiftLexicalLookup` results.
/// - `SwiftLexicalLookup` lookup - The names obtained from performing lookup with `SwiftLexicalLookup` are
///   mapped to `ConsumedLookupResult` common representation. The names receive flags
///   independently from `ASTScope` results.
/// - Flagging pass - The method iterates through both result arrays together and, taking into
///   account already applied flags and relationships between results, applies more flags to the results.
///
/// Fully flagged results, are then fed to the name matching pass. Using associated names, positions and
/// flags, it asserts the equality of results and produces console output.
///
/// Returns `true`, if the matching was successful and `false` otherwise.
/// Additionally, when matching fails, the function prints console output with the two results compared.
@_cdecl("swift_ASTGen_validateUnqualifiedLookup")
public func unqualifiedLookup(
  sourceFilePtr: UnsafeMutableRawPointer,
  astContext: BridgedASTContext,
  lookupAt: BridgedSourceLoc,
  finishInSequentialScope: Bool,
  astScopeResultRef: BridgedArrayRef
) -> Bool {
  // Obtain source file and lookup position
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  guard let sourceFileSyntax = sourceFile.pointee.syntax.as(SourceFileSyntax.self) else {
    print("Could not cast exported source file to SourceFileSyntax")
    return false
  }
  let sourceLocationConverter = sourceFile.pointee.sourceLocationConverter
  let configuredRegions = sourceFile.pointee.configuredRegions(astContext: astContext)
  
  let lookupCache: LookupCache
  
  if let sourceFileCache = sourceFile.pointee.lookupCache {
    lookupCache = sourceFileCache
  } else {
    lookupCache = LookupCache(capacity: 100, drop: 4)
    sourceFile.pointee.lookupCache = lookupCache
  }

  guard let lookupPosition = sourceFile.pointee.position(of: lookupAt),
    let lookupToken = sourceFileSyntax.token(at: lookupPosition)
  else {
    print("Could not determine lookup position")
    return false
  }

  // Map AST result
  let astResults = astConsumedResults(
    sourceFile: sourceFile,
    astScopeResultRef: astScopeResultRef
  )

  // Map SLL result
  let sllResults = sllConsumedResults(
    lookupToken: lookupToken,
    finishInSequentialScope: finishInSequentialScope,
    configuredRegions: configuredRegions,
    cache: lookupCache
  )

  // Add header to the output
  var consoleOutput =
    "-----> Lookup started at: \(sourceLocationConverter.location(for: lookupPosition).lineWithColumn) (\"\(lookupToken.text)\") finishInSequentialScope: \(finishInSequentialScope)\n"
  consoleOutput +=
    "     |" + "ASTScope".addPaddingUpTo(characters: rowCharWidth) + "|"
    + "SwiftLexicalLookup".addPaddingUpTo(characters: rowCharWidth) + "\n"

  // Flagging pass
  flaggingPass(
    astResults: astResults,
    sllResults: sllResults,
    sourceFileSyntax: sourceFileSyntax,
    lookupPosition: lookupPosition
  )

  // Matching pass
  let passed = matchingPass(
    astResults: astResults,
    sllResults: sllResults,
    sourceLocationConverter: sourceLocationConverter,
    consoleOutput: &consoleOutput
  )

  // Output
  if !passed {
    print(consoleOutput)
  }

  return passed
}

/// Check if the name at `namePosition`, was improperly introduced
/// by ASTScope (in the same declaration as lookup).
private func isInvalidFirstNameInDeclarationIntroduction(
  sourceFile: SourceFileSyntax,
  lookupPosition: AbsolutePosition,
  namePosition: AbsolutePosition
) -> Bool {
  func firstAncestorOfKind(
    of syntax: SyntaxProtocol?,
    kinds: [SyntaxProtocol.Type]
  ) -> SyntaxProtocol? {
    guard let syntax else { return nil }

    for kind in kinds {
      if syntax.is(kind) {
        return syntax
      }
    }

    return firstAncestorOfKind(of: syntax.parent, kinds: kinds)
  }

  let originToken = sourceFile.token(at: lookupPosition)
  let firstNameToken = sourceFile.token(at: namePosition)

  let commonAncestors: [SyntaxProtocol.Type] = [
    SwitchCaseSyntax.self,
    ClosureExprSyntax.self,
    AccessorDeclSyntax.self,
    AccessorBlockSyntax.self,
    ForStmtSyntax.self,
    PatternBindingSyntax.self,
  ]

  let originAncestor = firstAncestorOfKind(
    of: originToken,
    kinds: commonAncestors
  )

  let firstNameAncestor = firstAncestorOfKind(
    of: firstNameToken,
    kinds: commonAncestors
  )

  guard let originAncestor,
    let firstNameAncestor,
    originAncestor.kind == firstNameAncestor.kind
  else { return false }

  return originAncestor.kind == .patternBinding && originAncestor.id == firstNameAncestor.id
}

/// Returns consumed `ASTScope` results from the
/// given `astScopeResultRef`. Introduces appropriate flags.
private func astConsumedResults(
  sourceFile: UnsafePointer<ExportedSourceFile>,
  astScopeResultRef: BridgedArrayRef
) -> [ConsumedLookupResult] {
  let pointer = astScopeResultRef.data?.assumingMemoryBound(to: BridgedConsumedLookupResult.self)
  let count = astScopeResultRef.count

  let astScopeResultArray = Array(UnsafeBufferPointer(start: pointer, count: count))

  return astScopeResultArray.compactMap { bridgedResult in
    let identifierPointer = bridgedResult.name.raw?.assumingMemoryBound(to: CChar.self)

    guard let astResultPosition = sourceFile.pointee.position(of: bridgedResult.nameLoc) else {
      print("One of the results, doesn't have a position")
      return nil
    }

    let consumedResult = ConsumedLookupResult(
      rawName: identifierPointer == nil ? "" : String(cString: identifierPointer!),
      position: astResultPosition,
      flags: ConsumedLookupResultFlag(rawValue: bridgedResult.flag)
    )

    // If the name doesn't have any flags and
    // the name is empty, it should be omitted.
    if consumedResult.flags.isEmpty && consumedResult.name.isEmpty {
      consumedResult.flags.insert(.shouldBeOmitted)
    }

    return consumedResult
  }
}

/// Performs and returns `SwiftLexicalLookup` lookup and returns
/// the results an array of `ConsumedLookupResult`. Introduces appropriate flags.
private func sllConsumedResults(
  lookupToken: TokenSyntax,
  finishInSequentialScope: Bool,
  configuredRegions: ConfiguredRegions,
  cache: LookupCache
) -> [ConsumedLookupResult] {
  let resultsWithoutMacroReordering = lookupToken.lookup(
    nil,
    with: LookupConfig(finishInSequentialScope: finishInSequentialScope, configuredRegions: configuredRegions),
    cache: cache
  )
  cache.evictEntriesWithoutHit()

  // Early reordering of macro declaration parameters with its generic parameters.
  var results: [LookupResult] = []
  var previousMacroResult: LookupResult?

  for result in resultsWithoutMacroReordering {
    if let unwrappedMacroResult = previousMacroResult,
      result.scope.is(GenericParameterClauseSyntax.self)
    {
      results += [result, unwrappedMacroResult]
      previousMacroResult = nil
      continue
    } else if let unwrappedMacroResult = previousMacroResult {
      results.append(unwrappedMacroResult)
      previousMacroResult = nil
    }

    if result.scope.is(MacroDeclSyntax.self) {
      previousMacroResult = result
    } else {
      results.append(result)
    }
  }

  if let previousMacroResult {
    results.append(previousMacroResult)
  }

  return results.flatMap { result in
    switch result {
    case .lookForMembers(let syntax):
      return [
        ConsumedLookupResult(
          rawName: "",
          position: (syntax.asProtocol(SyntaxProtocol.self) as! LookInMembersScopeSyntax).lookupMembersPosition,
          flags: .shouldLookInMembers
        )
      ]
    case .lookForGenericParameters(let extensionDecl):
      return [
        ConsumedLookupResult(
          rawName: "",
          position: extensionDecl.extensionKeyword.positionAfterSkippingLeadingTrivia,
          flags: .ignoreNextFromHere
        )
      ]
    case .lookForImplicitClosureParameters(let closure):
      return [
        ConsumedLookupResult(
          rawName: "",
          position: closure.positionAfterSkippingLeadingTrivia,
          flags: .ignoreNextFromHere
        )
      ]
    default:
      if let parent = result.scope.parent, result.scope.is(GenericParameterClauseSyntax.self) {
        if let parentFunctionDecl = parent.as(FunctionDeclSyntax.self),
          parentFunctionDecl.attributes.range.contains(lookupToken.position)
        {
          // If lookup started from inside function attributes, don't reverse.
          return result.names.map { name in
            ConsumedLookupResult(rawName: name.identifier.name, position: name.position, flags: [])
          }
        } else if parent.is(FunctionDeclSyntax.self) || parent.is(SubscriptDeclSyntax.self)
          || result.scope.range.contains(lookupToken.position)
        {
          // If a result from function generic parameter clause or lookup started within it, reverse introduced names.
          return result.names.reversed().map { name in
            ConsumedLookupResult(
              rawName: name.identifier.name,
              position: name.position,
              flags: .placementRearranged
            )
          }
        } else if let nominalTypeScope = Syntax(parent).asProtocol(SyntaxProtocol.self) as? NominalTypeDeclSyntax,
          nominalTypeScope.inheritanceClause?.range.contains(lookupToken.position) ?? false
        {
          // If lookup started from nominal type inheritance clause, reverse introduced names.
          return result.names.reversed().map { name in
            ConsumedLookupResult(
              rawName: name.identifier.name,
              position: name.position,
              flags: .placementRearranged
            )
          }
        } else if let initializerDecl = parent.as(InitializerDeclSyntax.self),
          initializerDecl.range.contains(lookupToken.position)
        {
          // If lookup from inside the parent initializer decl, reverse introduced names.
          return result.names.reversed().map { name in
            ConsumedLookupResult(
              rawName: name.identifier.name,
              position: name.position,
              flags: .placementRearranged
            )
          }
        } else if let parentTypeAlias = parent.as(TypeAliasDeclSyntax.self),
          parentTypeAlias.initializer.range.contains(lookupToken.position)
        {
          // If lookup started from inside type alias initializer, reverse introduced names.
          return result.names.reversed().map { name in
            ConsumedLookupResult(
              rawName: name.identifier.name,
              position: name.position,
              flags: .placementRearranged
            )
          }
        }

        // No flags or reorderings to perform.
        return result.names.map { name in
          ConsumedLookupResult(rawName: name.identifier.name, position: name.position, flags: [])
        }
      } else {
        return result.names.map { name in
          // If a Self name not from protocol declaration, should be omitted if no match is found.
          let shouldBeOmitted = name.identifier.name == "Self" ? !result.scope.is(ProtocolDeclSyntax.self) : false

          return ConsumedLookupResult(
            rawName: name.identifier.name,
            position: name.position,
            flags: shouldBeOmitted ? [.shouldBeOptionallyOmitted] : []
          )
        }
      }
    }
  }
}

/// Adds all appropriate flags to `astResults` and `sllResults`.
private func flaggingPass(
  astResults: [ConsumedLookupResult],
  sllResults: [ConsumedLookupResult],
  sourceFileSyntax: SourceFileSyntax,
  lookupPosition: AbsolutePosition
) {
  var i = 0
  var astOffset = 0
  var sllOffset = 0
  var encounteredASTNames = Set<ConsumedLookupResult>()
  var ignoreAt: AbsolutePosition?
  var wasLookupStopped = false

  while i < max(astResults.count, sllResults.count) {
    var astResult: ConsumedLookupResult?

    if astOffset + i < astResults.count {
      astResult = astResults[astOffset + i]

      // Here only to not have to perform force unwraps later.
      guard let astResult else { break }

      // Check if lookup was stopped earlier. If so, flag this result with lookupStopped.
      if wasLookupStopped {
        astResult.flags.insert(.lookupStopped)
      }

      // Check if this is the end of ast lookup. If so, set wasLookupStopped to true.
      if astResult.isTheEndOfLookup {
        wasLookupStopped = true
      }

      // Check if this is not the first encounter of this ast name. If so, should be omitted.
      if !astResult.shouldLookInMembers {
        let isFirstEncounter = !encounteredASTNames.contains(astResult)

        if !isFirstEncounter {
          astResult.flags.insert(.shouldBeOmitted)
        }
      }

      // Check if names are being currently ignored from at this position. If so, should be omitted.
      if astResult.position == ignoreAt {
        astResult.flags.insert(.shouldBeOmitted)
      }

      // Check if this is an invalid introduction within the same declaration. If so, should be omitted.
      if isInvalidFirstNameInDeclarationIntroduction(
        sourceFile: sourceFileSyntax,
        lookupPosition: lookupPosition,
        namePosition: astResult.position
      ) && astResult.name != "self" {
        astResult.flags.insert(.shouldBeOmitted)
      }

      // Check if this name should be omitted. If so, continue the loop and add one to offset.
      if astResult.shouldBeOmitted {
        astOffset += 1
        continue
      }
    }

    if i + sllOffset < sllResults.count {
      let sllResult = sllResults[i + sllOffset]

      // Check if lookup was stopped earlier. If so, flag this result with lookupStopped.
      if wasLookupStopped && !(astResult?.isTheEndOfLookup ?? false) {
        sllResult.flags.insert(.lookupStopped)
      }

      if sllResult.shouldBeOptionallyOmitted {
        if let astResult,
          astResult.name == sllResult.name
        {
          sllResult.flags.remove(.shouldBeOptionallyOmitted)
        } else {
          sllResult.flags.insert(.shouldBeOmitted)
        }
      }

      // Check if next results at this position should be ignored. If so, set ignoreAt and omit this name.
      if sllResult.ignoreNextFromHere {
        ignoreAt = sllResult.position
        sllResult.flags.insert(.shouldBeOmitted)
      }

      // Check if this name should be omitted. If so, continue the loop and add one to offset.
      if sllResult.shouldBeOmitted {
        sllOffset += 1
        continue
      }
    }

    if let astResult {
      encounteredASTNames.insert(astResult)
    }

    i += 1
  }
}

/// Tries to match both results taking into account previously set
/// flags. Returns whether the test validation succeeded.
private func matchingPass(
  astResults: [ConsumedLookupResult],
  sllResults: [ConsumedLookupResult],
  sourceLocationConverter: SourceLocationConverter,
  consoleOutput: inout String
) -> Bool {
  var i = 0
  var astOffset = 0
  var sllOffset = 0
  var passed = true

  while i < max(astResults.count, sllResults.count) {
    var prefix = ""
    var astResultStr = ""
    var sllResultStr = ""

    var astResult: ConsumedLookupResult?

    if astOffset + i < astResults.count {
      astResult = astResults[astOffset + i]

      guard let astResult else { break }

      if astResult.shouldBeOmitted {
        consoleOutput +=
          "> ℹ️ | Omitted ASTScope name: \(astResult.consoleLogStr(sourceLocationConverter: sourceLocationConverter))\n"
        astOffset += 1
        continue
      }

      astResultStr += astResult.consoleLogStr(sourceLocationConverter: sourceLocationConverter)
    } else {
      astResultStr = "-----"
    }

    var sllResult: ConsumedLookupResult?

    if i + sllOffset < sllResults.count {
      sllResult = sllResults[i + sllOffset]

      guard let sllResult else { break }

      if sllResult.shouldBeOmitted {
        consoleOutput +=
          "> ℹ️ | Omitted SwiftLexicalLookup name: \(sllResult.consoleLogStr(sourceLocationConverter: sourceLocationConverter))\n"
        sllOffset += 1
        continue
      }

      sllResultStr = sllResult.consoleLogStr(sourceLocationConverter: sourceLocationConverter)
    } else {
      sllResultStr = "-----"
    }

    i += 1

    guard astResult != nil || sllResult != nil else { continue }

    if let astResult, let sllResult {
      if (astResult.position == sllResult.position && astResult.name == sllResult.name) {
        prefix = "✅"
      } else if astResult.lookupStopped || sllResult.lookupStopped {
        prefix = "⏩"
      } else if astResult.position == sllResult.position || astResult.name == sllResult.name {
        prefix = "⚠️"
        passed = false
      } else {
        prefix = "❌"
        passed = false
      }
    } else if (astResult?.lookupStopped ?? false) || (sllResult?.lookupStopped ?? false) {
      prefix = "⏩"
    } else {
      prefix = "❌"
      passed = false
    }

    consoleOutput +=
      "> \(prefix) |\(astResultStr.addPaddingUpTo(characters: rowCharWidth))|\(sllResultStr.addPaddingUpTo(characters: rowCharWidth))"

    consoleOutput += "\n"
  }

  return passed
}

/// Simple representation of lookup result.
/// Contains flags that indicate additional behaviour.
private class ConsumedLookupResult: Hashable {
  var rawName: String
  var position: AbsolutePosition
  var flags: ConsumedLookupResultFlag

  init(
    rawName: String,
    position: AbsolutePosition,
    flags: ConsumedLookupResultFlag
  ) {
    self.rawName = rawName
    self.position = position
    self.flags = flags
  }

  var name: String {
    shouldLookInMembers ? "" : rawName
  }

  var isTheEndOfLookup: Bool {
    flags.contains(.endOfLookup)
  }

  var shouldLookInMembers: Bool {
    flags.contains(.shouldLookInMembers)
  }

  var resultPlacementRearranged: Bool {
    flags.contains(.placementRearranged)
  }

  var shouldBeOmitted: Bool {
    flags.contains(.shouldBeOmitted)
  }

  var shouldBeOptionallyOmitted: Bool {
    flags.contains(.shouldBeOptionallyOmitted)
  }

  var ignoreNextFromHere: Bool {
    flags.contains(.ignoreNextFromHere)
  }

  var lookupStopped: Bool {
    flags.contains(.lookupStopped)
  }

  func consoleLogStr(sourceLocationConverter: SourceLocationConverter) -> String {
    (isTheEndOfLookup ? "End here: " : "") + (resultPlacementRearranged ? "↕️ " : "")
      + (ignoreNextFromHere ? "Ignore next from: " : "") + (shouldLookInMembers ? "Look memb: " : "\(name) ")
      + sourceLocationConverter.location(for: position).lineWithColumn
  }

  static func == (lhs: ConsumedLookupResult, rhs: ConsumedLookupResult) -> Bool {
    return lhs.rawName == rhs.rawName && lhs.position == rhs.position && lhs.flags == rhs.flags
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(rawName)
    hasher.combine(position)
    hasher.combine(flags)
  }
}

/// Determine behaviour during matching pass.
struct ConsumedLookupResultFlag: OptionSet, Hashable {
  let rawValue: Int

  /// Indicates lookup ended at this name. Continue with
  /// other names without matching and mark them as skipped.
  static let endOfLookup = ConsumedLookupResultFlag(rawValue: 1 << 0)
  /// This name prompts client to look in members of associated scope.
  static let shouldLookInMembers = ConsumedLookupResultFlag(rawValue: 1 << 1)
  /// The original position in result of this name
  /// might be different than displayed.
  static let placementRearranged = ConsumedLookupResultFlag(rawValue: 1 << 2)
  /// The name should be ignored.
  static let shouldBeOmitted = ConsumedLookupResultFlag(rawValue: 1 << 3)
  /// If no match is found, this name should be ignored.
  static let shouldBeOptionallyOmitted = ConsumedLookupResultFlag(rawValue: 1 << 4)
  /// Means that one of the previous
  /// names indicated the end of lookup.
  static let lookupStopped = ConsumedLookupResultFlag(rawValue: 1 << 5)
  /// Next names from associated position should be omitted.
  /// Filtering is applied until then next name of this kind is found and
  /// position used for ignoring is updated.
  static let ignoreNextFromHere = ConsumedLookupResultFlag(rawValue: 1 << 6)
}

extension SourceLocation {
  fileprivate var lineWithColumn: String {
    return "\(line):\(column)"
  }
}

extension String {
  fileprivate func addPaddingUpTo(characters charCount: Int) -> String {
    guard self.count < charCount else { return self }

    let lengthDifference = charCount - self.count

    var leftPad = ""
    var rightPad = ""

    for _ in 0..<(lengthDifference / 2) {
      leftPad += " "
    }

    for _ in 0..<((lengthDifference + 1) / 2) {
      rightPad += " "
    }

    return leftPad + self + rightPad
  }
}
