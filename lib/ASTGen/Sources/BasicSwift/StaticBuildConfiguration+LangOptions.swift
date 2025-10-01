//===--- StaticBuildConfiguration+ASTContext.swift ------------------------===//
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
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling
import SwiftDiagnostics
@_spi(Compiler) import SwiftIfConfig
@_spi(ExperimentalLanguageFeatures) import SwiftParser
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

extension StaticBuildConfiguration {
  /// Configuration entries captured in the initializer from a bridged context.
  private struct ConfigurationEntries {
    var customConditions: Set<String> = []
    var features: Set<String> = []
    var attributes: Set<String> = []
    var targetOSNames: Set<String> = []
    var targetArchitectures: Set<String> = []
    var targetEnvironments: Set<String> = []
    var targetRuntimes: Set<String> = []
    var targetPointerAuthenticationSchemes: Set<String> = []
    var targetObjectFileFormats: Set<String> = []
  }

  /// Initialize a static build configuration for the given language options.
  init(langOptions: BridgedLangOptions) {
    var entries = ConfigurationEntries()

    langOptions.enumerateBuildConfigurationEntries(callbackContext: &entries) { cContext, entries, key, value in
      let entries = entries.assumingMemoryBound(to: ConfigurationEntries.self)
      switch key {
      case .BCKAttribute:
        entries.pointee.attributes.insert(String(bridged: value))
      case .BCKCustomCondition:
        entries.pointee.customConditions.insert(String(bridged: value))
      case .BCKFeature:
        entries.pointee.features.insert(String(bridged: value))
      case .BCKTargetOSName:
        entries.pointee.targetOSNames.insert(String(bridged: value))
      case .BCKTargetArchitecture:
        entries.pointee.targetArchitectures.insert(String(bridged: value))
      case .BCKTargetEnvironment:
        entries.pointee.targetEnvironments.insert(String(bridged: value))
      case .BCKTargetRuntime:
        entries.pointee.targetRuntimes.insert(String(bridged: value))
      case .BCKTargetPointerAuthenticationScheme:
        entries.pointee.targetPointerAuthenticationSchemes.insert(String(bridged: value))
      case .BCKTargetObjectFileFormat:
        entries.pointee.targetObjectFileFormats.insert(String(bridged: value))
      }
    }

    let targetPointerBitWidth = Int(langOptions.targetPointerBitWidth)

    // Atomic bit widths.
    let targetAtomicBitWidths: [Int]
    do {
      var bitWidthsBuf: UnsafeMutablePointer<SwiftInt>? = nil
      let bitWidthsCount = langOptions.getTargetAtomicBitWidths(&bitWidthsBuf)
      targetAtomicBitWidths = Array(UnsafeMutableBufferPointer(start: bitWidthsBuf, count: bitWidthsCount))
      deallocateIntBuffer(bitWidthsBuf);
    }

    let endianness: Endianness
    switch langOptions.targetEndianness {
    case .EndianBig: endianness = .big
    case .EndianLittle: endianness = .little
    }

    let languageVersion = getVersionTuple { langOptions.getLanguageVersion(&$0) }
    let compilerVersion = getVersionTuple { langOptions.getCompilerVersion(&$0) }
    self.init(
      customConditions: entries.customConditions,
      features: entries.features,
      attributes: entries.attributes,
      targetOSs: entries.targetOSNames,
      targetArchitectures: entries.targetArchitectures,
      targetEnvironments: entries.targetEnvironments,
      targetRuntimes: entries.targetRuntimes,
      targetPointerAuthenticationSchemes: entries.targetPointerAuthenticationSchemes,
      targetObjectFileFormats: entries.targetObjectFileFormats,
      targetPointerBitWidth: targetPointerBitWidth,
      targetAtomicBitWidths: targetAtomicBitWidths,
      endianness: endianness,
      languageVersion: languageVersion,
      compilerVersion: compilerVersion
    )
  }
}

/// Get a version tuple from C++.
private func getVersionTuple(
  body: (inout UnsafeMutablePointer<SwiftInt>?) -> Int
) -> VersionTuple {
  var componentsBuf: UnsafeMutablePointer<SwiftInt>? = nil
  let count = body(&componentsBuf)
  let version = VersionTuple(
    components: Array(UnsafeMutableBufferPointer(start: componentsBuf, count: count))
  )
  deallocateIntBuffer(componentsBuf);
  return version
}

@_cdecl("swift_ASTGen_printStaticBuildConfiguration")
public func printStaticBuildConfiguration(
  langOptions: BridgedLangOptions
) -> BridgedStringRef {
  let config = StaticBuildConfiguration(langOptions: langOptions)
  let result = try? JSON.encode(config).withUnsafeBufferPointer { buffer in
    let ptr = UnsafeMutablePointer<UInt8>.allocate(
      capacity: buffer.count + 1
    )
    if let baseAddress = buffer.baseAddress {
      ptr.initialize(from: baseAddress, count: buffer.count)
    }

    // null terminate, for client's convenience.
    ptr[buffer.count] = 0

    return BridgedStringRef(data: ptr, count: buffer.count)
  }

  return result ?? BridgedStringRef()
}

@_cdecl("swift_Basic_createStaticBuildConfiguration")
public func createStaticBuildConfiguration(
  cLangOpts: BridgedLangOptions
) -> UnsafeMutableRawPointer {
  let storage = UnsafeMutablePointer<StaticBuildConfiguration>.allocate(capacity: 1)
  storage.initialize(to: StaticBuildConfiguration(langOptions: cLangOpts))
  return UnsafeMutableRawPointer(storage)
}

/// Free the given static build configuration.
@_cdecl("swift_Basic_freeStaticBuildConfiguration")
public func freeStaticBuildConfiguration(pointer: UnsafeMutableRawPointer) {
  pointer.assumingMemoryBound(to: StaticBuildConfiguration.self)
    .deinitialize(count: 1).deallocate()
}
