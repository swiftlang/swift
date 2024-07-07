//===--- CompilerBuildConfiguration.swift --------------\------------------===//
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
import SwiftIfConfig
import SwiftSyntax

/// A build configuration that uses the compiler's ASTContext to answer
/// queries.
struct CompilerBuildConfiguration: BuildConfiguration {
  let ctx: BridgedASTContext

  func isCustomConditionSet(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsCustomConditionSet(nameRef)
    }
  }
  
  func hasFeature(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsHasFeatureNamed(nameRef)
    }
  }
  
  func hasAttribute(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsHasAttributeNamed(nameRef)
    }
  }
  
  func canImport(importPath: [String], version: CanImportVersion) throws -> Bool {
    var importPathStr = importPath.joined(separator: ".")

    var versionComponents: [Int]
    let cVersionKind: BridgedCanImportVersion
    switch version {
    case .unversioned:
      cVersionKind = .CanImportUnversioned
      versionComponents = []

    case .version(let versionTuple):
      cVersionKind = .CanImportVersion
      versionComponents = versionTuple.components

    case .underlyingVersion(let versionTuple):
      cVersionKind = .CanImportUnderlyingVersion
      versionComponents = versionTuple.components
    }

    return importPathStr.withBridgedString { bridgedImportPathStr in
      versionComponents.withUnsafeBufferPointer { versionComponentsBuf in
        ctx.canImport(
          importPath: bridgedImportPathStr,
          versionKind: cVersionKind,
          versionComponents: versionComponentsBuf.baseAddress,
          numVersionComponents: versionComponentsBuf.count
        )
      }
    }
  }
  
  func isActiveTargetOS(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetOS(nameRef)
    }
  }
  
  func isActiveTargetArchitecture(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetArchitecture(nameRef)
    }
  }
  
  func isActiveTargetEnvironment(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetEnvironment(nameRef)
    }
  }
  
  func isActiveTargetRuntime(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetRuntime(nameRef)
    }
  }
  
  func isActiveTargetPointerAuthentication(name: String) throws -> Bool {
    var name = name
    return name.withBridgedString { nameRef in
      ctx.langOptsIsActiveTargetPtrAuth(nameRef)
    }
  }
  
  var targetPointerBitWidth: Int {
    Int(ctx.langOptsTargetPointerBitWidth)
  }

  var targetAtomicBitWidths: [Int] {
    var bitWidthsBuf: UnsafeMutablePointer<SwiftInt>? = nil
    let count = ctx.langOptsGetTargetAtomicBitWidths(&bitWidthsBuf)
    let bitWidths = Array(UnsafeMutableBufferPointer(start: bitWidthsBuf, count: count))
    bitWidthsBuf?.deallocate()
    return bitWidths
  }

  var endianness: Endianness {
    switch ctx.langOptsTargetEndianness {
    case .EndianBig: return .big
    case .EndianLittle: return .little
    }
  }

  var languageVersion: VersionTuple { 
    var componentsBuf: UnsafeMutablePointer<SwiftInt>? = nil
    let count = ctx.langOptsGetLanguageVersion(&componentsBuf)
    let version = VersionTuple(
      components: Array(UnsafeMutableBufferPointer(start: componentsBuf, count: count))
    )
    componentsBuf?.deallocate()
    return version
  }

  var compilerVersion: VersionTuple { 
    var componentsBuf: UnsafeMutablePointer<SwiftInt>? = nil
    let count = ctx.langOptsGetCompilerVersion(&componentsBuf)
    let version = VersionTuple(
      components: Array(UnsafeMutableBufferPointer(start: componentsBuf, count: count))
    )
    componentsBuf?.deallocate()
    return version
  }
}

/// Extract the #if clause range information for the given source file.
@_cdecl("swift_ASTGen_configuredRegions")
public func configuredRegions(
  astContext: BridgedASTContext,
  sourceFilePtr: UnsafeRawPointer,
  cRegionsOut: UnsafeMutablePointer<UnsafeMutablePointer<BridgedIfConfigClauseRangeInfo>?>
) -> Int {
  let sourceFilePtr = sourceFilePtr.bindMemory(to: ExportedSourceFile.self, capacity: 1)
  let configuration = CompilerBuildConfiguration(ctx: astContext)
  let regions = sourceFilePtr.pointee.syntax.configuredRegions(in: configuration)

  var cRegions: [BridgedIfConfigClauseRangeInfo] = []
  for (ifConfig, state) in regions {
    let kind: BridgedIfConfigClauseKind
    switch state {
    case .active: kind = .IfConfigActive
    case .inactive, .unparsed: kind = .IfConfigInactive
    }

    let bodyLoc: AbsolutePosition
    if let elements = ifConfig.elements {
      bodyLoc = elements.position
    } else if let condition = ifConfig.condition {
      bodyLoc = condition.endPosition
    } else {
      bodyLoc = ifConfig.endPosition
    }

    cRegions.append(
      .init(
        directiveLoc: sourceFilePtr.pointee.sourceLoc(
          at: ifConfig.poundKeyword.positionAfterSkippingLeadingTrivia
        ),
        bodyLoc: sourceFilePtr.pointee.sourceLoc(at: bodyLoc),
        endLoc: sourceFilePtr.pointee.sourceLoc(
          at: ifConfig.endPosition
        ),
        kind: kind
      )
    )
  }

  let cRegionsBuf: UnsafeMutableBufferPointer<BridgedIfConfigClauseRangeInfo> =
    .allocate(capacity: cRegions.count)
  _ = cRegionsBuf.initialize(from: cRegions)
  cRegionsOut.pointee = cRegionsBuf.baseAddress
  return cRegionsBuf.count
}
