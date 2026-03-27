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
import SwiftBasicFormat
import SwiftSyntax
import swiftASTGen

/// Minimize parsed source for .swiftinterface generation.
/// Takes an `ExportedSourceFile*` from `swift_ASTGen_parseSourceFile`.
/// When `internalImportByDefault` is true, bare imports (without an explicit
/// access modifier) are treated as internal and removed.
/// When `removeInternalDecls` is true, internal/fileprivate declarations are
/// removed; when false only function/accessor bodies are stripped.
/// The result is heap-allocated; free with `swift_ASTGen_freeBridgedString`.
@_cdecl("swift_ASTGen_minimizeSourceForInterface")
public func minimizeSourceForInterface(
  sourceFilePtr: UnsafeMutablePointer<UInt8>,
  internalImportByDefault: Bool,
  removeInternalDecls: Bool,
  resultOut: UnsafeMutablePointer<BridgedStringRef>
) {
  sourceFilePtr.withMemoryRebound(to: ExportedSourceFile.self, capacity: 1) { sf in
    let minimized = sf.pointee.syntax.minimizedForInterface(
      internalImportByDefault: internalImportByDefault,
      removeInternalDecls: removeInternalDecls
    )
    resultOut.pointee = allocateBridgedString(minimized.formatted().description)
  }
}

/// Extract only import statements from parsed source.
/// Takes an `ExportedSourceFile*` from `swift_ASTGen_parseSourceFile`.
/// The result is heap-allocated; free with `swift_ASTGen_freeBridgedString`.
@_cdecl("swift_ASTGen_extractImports")
public func extractImports(
  sourceFilePtr: UnsafeMutablePointer<UInt8>,
  resultOut: UnsafeMutablePointer<BridgedStringRef>
) {
  sourceFilePtr.withMemoryRebound(to: ExportedSourceFile.self, capacity: 1) { sf in
    let minimized = sf.pointee.syntax.minimizedToImportsOnly()
    resultOut.pointee = allocateBridgedString(minimized.formatted().description)
  }
}
