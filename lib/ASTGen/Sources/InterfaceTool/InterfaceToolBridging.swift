//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import BasicBridging
import SwiftSyntax
import swiftASTGen

/// Extract imports and other dependency-scanning-relevant source code from an
/// \c ExportedSourceFile.
///
/// The result must be freed with swift_ASTGen_freeBridgedString.
@_cdecl("swift_ASTGen_minimizeForDependencyScan")
public func minimizeForDependencyScan(
  sourceFilePtr: UnsafeMutablePointer<UInt8>,
  resultOut: UnsafeMutablePointer<BridgedStringRef>
) {
  sourceFilePtr.withMemoryRebound(to: ExportedSourceFile.self, capacity: 1) { sf in
    let minimized = sf.pointee.syntax.minimizeForDependencyScan()
    resultOut.pointee = allocateBridgedString(minimized)
  }
}
