//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_PLUGIN_SERVER_SUPPORTS_WASM

@testable import swift_plugin_server
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

/// A scriptable `WasmPlugin` that records every decoded request and replies with
/// canned, message-type-specific JSON. Reference-typed so recorded state survives
/// being stored as `any WasmPlugin` in the handler.
final class MockWasmPlugin: WasmPlugin {
  /// Protocol version this mock guest reports in its `getCapabilityResult`.
  let reportedProtocolVersion: Int
  /// When false, the mock answers `getCapability` with an `expandMacroResult`,
  /// simulating a guest that cannot perform the handshake.
  let answersCapability: Bool
  let shutDownError: (any Error)?
  let expandError: (any Error)?
  /// Every request the mock decoded, in order.
  private(set) var receivedMessages: [HostToPluginMessage] = []
  /// The protocol version of each `getCapability` the mock received, or nil for one
  /// without a host capability.
  private(set) var forwardedProtocolVersions: [Int?] = []
  private(set) var shutDownCount = 0

  init(
    reportedProtocolVersion: Int,
    answersCapability: Bool = true,
    shutDownError: (any Error)? = nil,
    expandError: (any Error)? = nil
  ) {
    self.reportedProtocolVersion = reportedProtocolVersion
    self.answersCapability = answersCapability
    self.shutDownError = shutDownError
    self.expandError = expandError
  }

  func handleMessage(_ json: [UInt8]) throws -> [UInt8] {
    let message = try json.withUnsafeBufferPointer {
      try JSON.decode(HostToPluginMessage.self, from: $0)
    }
    receivedMessages.append(message)
    if case .getCapability = message {
      let request = try json.withUnsafeBufferPointer {
        try JSON.decode(GetCapabilityMessage.self, from: $0)
      }
      forwardedProtocolVersions.append(request.getCapability.capability?.protocolVersion)
    }
    if let expandError, Self.expandedModuleName(of: message) != nil {
      throw expandError
    }

    let response: PluginToHostMessage
    switch message {
    case .getCapability where answersCapability:
      response = .getCapabilityResult(
        capability: PluginMessage.PluginCapability(
          protocolVersion: reportedProtocolVersion,
          features: ["load-plugin-library"]
        )
      )
    default:
      // Expand messages get a canned result with a distinct sentinel. When
      // answersCapability is false, getCapability falls here too.
      response = .expandMacroResult(expandedSource: "FORWARDED", diagnostics: [])
    }
    return try JSON.encode(response)
  }

  func shutDown() throws {
    shutDownCount += 1
    if let shutDownError {
      throw shutDownError
    }
  }

  /// Whether the mock was asked to expand a macro.
  var receivedExpand: Bool {
    !expandedModuleNames.isEmpty
  }

  var expandedModuleNames: [String] {
    receivedMessages.compactMap(Self.expandedModuleName(of:))
  }

  private static func expandedModuleName(of message: HostToPluginMessage) -> String? {
    switch message {
    case let .expandAttachedMacro(macro, _, _, _, _, _, _, _, _, _), let .expandFreestandingMacro(macro, _, _, _, _, _):
      return macro.moduleName
    default:
      return nil
    }
  }
}

// `HostCapability.protocolVersion` is internal to swift-syntax.
private struct GetCapabilityMessage: Decodable {
  struct Payload: Decodable {
    struct Capability: Decodable {
      let protocolVersion: Int
    }
    let capability: Capability?
  }
  let getCapability: Payload
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
