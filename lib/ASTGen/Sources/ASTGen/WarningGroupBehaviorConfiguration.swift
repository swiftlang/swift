//===--- WarningGroupBehaviorConfiguration.swift --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import BasicBridging
import ASTBridging
import swiftBasicSwift
import SwiftDiagnostics
@_spi(ExperimentalLanguageFeatures) import SwiftWarningControl
@_spi(ExperimentalLanguageFeatures) import SwiftParser
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

@_cdecl("swift_ASTGen_warningGroupBehaviorAtPosition")
public func warningGroupBehaviorAtPosition(
  sourceFilePtr: UnsafeMutableRawPointer,
  globalRules: BridgedArrayRef,
  diagnosticGroupNameStrRef: BridgedStringRef,
  loc: SourceLoc) -> swift.WarningGroupBehavior {
    let sourceFilePtr = sourceFilePtr.bindMemory(to: ExportedSourceFile.self, capacity: 1)
    let regions = sourceFilePtr.pointee.warningGroupControlRegionTree(globalRules: globalRules)

    let diagnosticGroupIdentifier = DiagnosticGroupIdentifier(String(bridged: diagnosticGroupNameStrRef))
    guard let position = sourceFilePtr.pointee.position(of: loc) else {
      return swift.WarningGroupBehavior.none
    }

    guard let control = regions.warningGroupControl(at: position,
                                                    for: diagnosticGroupIdentifier) else {
      return swift.WarningGroupBehavior.none
    }

    switch control {
      case .error: return swift.WarningGroupBehavior.error
      case .warning: return swift.WarningGroupBehavior.warning
      case .ignored: return swift.WarningGroupBehavior.ignored
    }
}

@_cdecl("swift_ASTGen_isWarningGroupEnabledInFile")
public func isWarningGroupEnabledInFile(
  sourceFilePtr: UnsafeMutableRawPointer,
  globalRules: BridgedArrayRef,
  diagnosticGroupNameStrRef: BridgedStringRef) -> Bool {
    let sourceFilePtr = sourceFilePtr.bindMemory(to: ExportedSourceFile.self, capacity: 1)
    let regions = sourceFilePtr.pointee.warningGroupControlRegionTree(globalRules: globalRules)
    let diagnosticGroupIdentifier = DiagnosticGroupIdentifier(String(bridged: diagnosticGroupNameStrRef))
    return regions.enabledGroups.contains(diagnosticGroupIdentifier)
}

extension ExportedSourceFile {
  /// Return the warning group behavior regions for this source file.
  mutating func warningGroupControlRegionTree(globalRules: BridgedArrayRef) -> WarningControlRegionTree {
    if let _warningControlRegionTree {
      return _warningControlRegionTree
    }

    // Bridge the global configuration (`-Werror`, `-Wwarning`, etc.)
    var globalGroupRules: [(DiagnosticGroupIdentifier, WarningGroupControl)] = []
    let pointer = globalRules.data?.assumingMemoryBound(to: BridgedWarningGroupBehaviorRule.self)
    let globalRuleArray = Array(UnsafeBufferPointer(start: pointer, count: globalRules.count))
    for rule in globalRuleArray {
      let groupIdentifier = DiagnosticGroupIdentifier(String(bridged: rule.getGroupName()))
      let groupBehavior = switch rule.getBehavior() {
        case .error: WarningGroupControl.error
        case .warning: WarningGroupControl.warning
        case .ignored: WarningGroupControl.ignored
        case .none: fatalError("unexpected 'none' warning group behavior control")
      }
      globalGroupRules.append((groupIdentifier, groupBehavior))
    }

    // Bridge the compiler's diagnostic group links
    var subGroups: [DiagnosticGroupIdentifier : [DiagnosticGroupIdentifier]] = [:]
    for i in 0..<getDiagnosticGroupLinksCount() {
      let linkPair = getDiagnosticGroupLink(at: i)
      let superGroupID = DiagnosticGroupIdentifier(String(bridged: linkPair.first))
      let subGroupID = DiagnosticGroupIdentifier(String(bridged: linkPair.second))
      if subGroups[superGroupID] == nil {
        subGroups[superGroupID] = [subGroupID]
      } else {
        subGroups[superGroupID]!.append(subGroupID)
      }
    }
    let groupInheritanceTree = (try? DiagnosticGroupInheritanceTree(subGroups: subGroups)) ?? nil
    let regions = syntax.warningGroupControlRegionTree(globalControls: globalGroupRules,
                                                       groupInheritanceTree: groupInheritanceTree)
    _warningControlRegionTree = regions
    return regions
  }
}
