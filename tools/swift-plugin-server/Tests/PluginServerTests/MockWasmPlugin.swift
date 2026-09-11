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

@testable import swift_plugin_server
import SystemPackage
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
  /// Every request the mock decoded, in order.
  private(set) var receivedMessages: [HostToPluginMessage] = []

  init(reportedProtocolVersion: Int, answersCapability: Bool = true) {
    self.reportedProtocolVersion = reportedProtocolVersion
    self.answersCapability = answersCapability
  }

  // Required by `WasmPlugin`; unused (tests inject instances via a factory closure).
  init(path: FilePath) throws {
    self.reportedProtocolVersion = PluginMessage.PROTOCOL_VERSION_NUMBER
    self.answersCapability = true
  }

  func handleMessage(_ json: [UInt8]) throws -> [UInt8] {
    let message = try json.withUnsafeBufferPointer {
      try JSON.decode(HostToPluginMessage.self, from: $0)
    }
    receivedMessages.append(message)

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

  func shutDown() throws {}

  /// Whether the mock received a `getCapability` carrying a non-nil host capability.
  /// (`HostCapability.protocolVersion` is internal to swift-syntax and unreadable here,
  /// so we assert presence rather than the value.)
  var receivedForwardedCapability: Bool {
    receivedMessages.contains { message in
      if case .getCapability(let capability) = message { return capability != nil }
      return false
    }
  }

  /// Whether the mock was asked to expand a macro.
  var receivedExpand: Bool {
    receivedMessages.contains { message in
      switch message {
      case .expandAttachedMacro, .expandFreestandingMacro: return true
      default: return false
      }
    }
  }
}
