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

@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

/// Mock handler for tests recording all received messages.
final class MockBaseHandler: PluginMessageHandler {
  let failsLoads: Bool
  private(set) var receivedMessages: [HostToPluginMessage] = []
  private(set) var shutDownCount = 0

  init(failsLoads: Bool = false) {
    self.failsLoads = failsLoads
  }

  func handleMessage(_ message: HostToPluginMessage) -> PluginToHostMessage {
    receivedMessages.append(message)
    switch message {
    case .getCapability:
      return .getCapabilityResult(
        capability: PluginMessage.PluginCapability(
          protocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER,
          features: ["load-plugin-library"]
        )
      )
    case .loadPluginLibrary:
      return .loadPluginLibraryResult(loaded: !failsLoads, diagnostics: [])
    case .expandAttachedMacro, .expandFreestandingMacro:
      return .expandMacroResult(expandedSource: nil, diagnostics: [])
    }
  }
  func shutDown() throws {
    shutDownCount += 1
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
