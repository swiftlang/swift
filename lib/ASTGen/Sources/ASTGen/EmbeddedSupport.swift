//===--- EmbeddedSupport.swift --------------------------------------------===//
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

import ASTBridging
import BasicBridging
import SwiftIfConfig
import SwiftSyntax

extension ExportedSourceFile {
  /// Return the configured regions for this source file.
  mutating func configuredRegionsIfEmbedded(astContext: BridgedASTContext) -> ConfiguredRegions {
    if let _configuredRegionsIfEmbedded {
      return _configuredRegionsIfEmbedded
    }

    // Ensure that we've already computed the configured regions, which can
    // emit diagnostics.
    _ = configuredRegions(astContext: astContext)

    let configuration = EmbeddedBuildConfiguration(
      ctx: astContext,
      sourceBuffer: buffer
    )

    let regions = syntax.configuredRegions(in: configuration)
    _configuredRegionsIfEmbedded = regions
    return regions
  }
}

/// Determine whether the given source location is active when this source
/// file is compiled in Embedded Swift.
@_cdecl("swift_ASTGen_activeInEmbeddedSwift")
public func activeInEmbeddedSwift(
  astContext: BridgedASTContext,
  sourceFilePtr: UnsafeMutableRawPointer,
  at location: SourceLoc
) -> Int {
  guard let token = findToken(
    in: UnsafeRawPointer(sourceFilePtr),
    at: location.raw?.assumingMemoryBound(to: UInt8.self)
  ) else {
    return 0
  }

  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  let regions = sourceFile.pointee.configuredRegionsIfEmbedded(astContext: astContext)
  return regions.isActive(token) == .active ? 1 : 0
}

/// A build configuration that is a thin shim over the
/// CompilerBuildConfiguration that enables the Embedded language feature,
/// allowing the compiler to determine whether code we're in will be available
/// in an embedded context or not.
struct EmbeddedBuildConfiguration: BuildConfiguration {
  let configuration: CompilerBuildConfiguration

  init(ctx: BridgedASTContext, sourceBuffer: UnsafeBufferPointer<UInt8>) {
    self.configuration = .init(ctx: ctx, sourceBuffer: sourceBuffer)
  }

  func isCustomConditionSet(name: String) throws -> Bool {
    // $Embedded is set when building Embedded Swift
    if name == "$Embedded" {
      return true
    }

    return try configuration.isCustomConditionSet(name: name)
  }

  func hasFeature(name: String) throws -> Bool {
    // The "Embedded" feature is set when building Embedded Swift.
    if name == "Embedded" {
      return true
    }

    return try configuration.hasFeature(name: name)
  }

  func hasAttribute(name: String) throws -> Bool {
    return try configuration.hasAttribute(name: name)
  }

  func canImport(
    importPath: [(TokenSyntax, String)],
    version: CanImportVersion
  ) throws -> Bool {
    // FIXME: We would prefer to call the underlying canImport in some kind of
    // "replay" mode that is not allowed to produce diagnostics or trigger any
    // loading.
    return false
  }

  func isActiveTargetOS(name: String) throws -> Bool {
    return try configuration.isActiveTargetOS(name: name)
  }

  func isActiveTargetArchitecture(name: String) throws -> Bool {
    return try configuration.isActiveTargetArchitecture(name: name)
  }

  func isActiveTargetEnvironment(name: String) throws -> Bool {
    return try configuration.isActiveTargetEnvironment(name: name)
  }

  func isActiveTargetRuntime(name: String) throws -> Bool {
    return try configuration.isActiveTargetRuntime(name: name)
  }

  func isActiveTargetPointerAuthentication(name: String) throws -> Bool {
    return try configuration.isActiveTargetPointerAuthentication(name: name)
  }

  var targetPointerBitWidth: Int {
    return configuration.targetPointerBitWidth
  }

  var targetAtomicBitWidths: [Int] {
    return configuration.targetAtomicBitWidths
  }

  var endianness: Endianness {
    return configuration.endianness
  }

  var languageVersion: VersionTuple {
    return configuration.languageVersion
  }

  var compilerVersion: VersionTuple {
    return configuration.compilerVersion
  }
}
