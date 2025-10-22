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
  astContext: BridgedASTContext,
  sourceFilePtr: UnsafeMutableRawPointer,
  diagnosticGroupNameStrRef: BridgedStringRef,
  loc: SourceLoc) -> BridgedWarningGroupBehavior {
    let sourceFilePtr = sourceFilePtr.bindMemory(to: ExportedSourceFile.self, capacity: 1)
    let regions = sourceFilePtr.pointee.warningGroupControlRegionTree(astContext: astContext)
    
    let diagnosticGroupIdentifier = DiagnosticGroupIdentifier(String(bridged: diagnosticGroupNameStrRef))
    guard let position = sourceFilePtr.pointee.position(of: loc) else {
      return BridgedWarningGroupBehavior.WarningGroupBehaviorNone
    }
    
    guard let behavior = regions.warningGroupBehavior(at: position,
                                                      for: diagnosticGroupIdentifier) else {
      return BridgedWarningGroupBehavior.WarningGroupBehaviorNone
    }
    
    switch behavior {
      case .error:
        return BridgedWarningGroupBehavior.WarningGroupBehaviorError
      case .warning:
        return BridgedWarningGroupBehavior.WarningGroupBehaviorWarning
      case .ignored:
        return BridgedWarningGroupBehavior.WarningGroupBehaviorIgnored
    }
}

extension ExportedSourceFile {
  /// Return the warning group behavior regions for this source file.
  mutating func warningGroupControlRegionTree(astContext: BridgedASTContext) -> WarningControlRegionTree {
    if let _warningControlRegionTree {
      return _warningControlRegionTree
    }

    let regions = syntax.warningGroupControlRegionTree
    _warningControlRegionTree = regions
    return regions
  }
}
