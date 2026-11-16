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
import Testing
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

private let moduleName = "MacroDefinition"

private let accessorSkewMessage = """
  cannot expand macro 'constInt' from 'MacroDefinition.ConstMacro': the Wasm macro plugin reports \
  protocol version 7, but the host requires protocol version 8 to send 'accessor' syntax; rebuild \
  the plugin against a newer swift-syntax
  """

@Suite
struct WasmCapabilityForwardingTests {
  /// Builds a handler injected with `mock`, then drives the real handshake:
  /// optional host `getCapability`, then `loadPluginLibrary` for the `.wasm` guest.
  private func handlerLoadingMock(
    _ mock: MockWasmPlugin,
    sendGetCapability: Bool = true,
    hostProtocolVersion: Int = PluginMessage.PROTOCOL_VERSION_NUMBER
  ) -> (WasmInterceptingMessageHandler<MockBaseHandler>, PluginToHostMessage) {
    let handler = WasmInterceptingMessageHandler(
      base: MockBaseHandler(),
      makePlugin: { _ in mock }
    )
    if sendGetCapability {
      _ = handler.handleMessage(
        .getCapability(capability: PluginMessage.HostCapability(protocolVersion: hostProtocolVersion))
      )
    }
    let loadResult = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: moduleName)
    )
    return (handler, loadResult)
  }

  // The guest must receive a forwarded `getCapability` (carrying the host version) on load.
  @Test
  func forwardsHostCapabilityToGuestOnLoad() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
    let (_, loadResult) = handlerLoadingMock(mock, hostProtocolVersion: 7)

    try #require(loadResult.isSuccessfulLoad)
    #expect(mock.receivedMessages.count == 1)
    #expect(mock.forwardedProtocolVersions == [7])
    if case .getCapability = mock.receivedMessages.first {} else {
      Issue.record("first guest message should be getCapability, got \(String(describing: mock.receivedMessages.first))")
    }
  }

  // A protocol-7 guest receiving an `.accessor` declSyntax gets a skew error at the attribute, not the forward.
  @Test
  func diagnosesAccessorSkew_inDeclSyntax() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: 7)
    let (handler, loadResult) = handlerLoadingMock(mock)
    try #require(loadResult.isSuccessfulLoad)

    let response = handler.handleMessage(.attachedExpansion(moduleName: moduleName, declKind: .accessor))

    guard case .expandMacroResult(let expandedSource, let diagnostics) = response else {
      Issue.record("expected expandMacroResult, got \(response)")
      return
    }
    #expect(expandedSource == nil)
    #expect(diagnostics.map(\.message) == [accessorSkewMessage])
    #expect(diagnostics.map(\.severity) == [.error])
    #expect(diagnostics.map(\.position.fileName) == ["test.swift"])
    #expect(diagnostics.map(\.position.offset) == [0])
    #expect(!mock.receivedExpand)
  }

  // `.accessor` in lexicalContext, not just the root syntax, must also be caught.
  @Test(arguments: [false, true])
  func diagnosesAccessorSkew_inLexicalContext(attached: Bool) throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: 7)
    let (handler, loadResult) = handlerLoadingMock(mock)
    try #require(loadResult.isSuccessfulLoad)
    let lexicalContext: [PluginMessage.Syntax] = [.fixture(kind: .accessor, source: "get")]
    let request: HostToPluginMessage = attached
      ? .attachedExpansion(moduleName: moduleName, declKind: .declaration, lexicalContext: lexicalContext)
      : .freestandingExpansion(moduleName: moduleName, syntaxKind: .expression, lexicalContext: lexicalContext)

    let response = handler.handleMessage(request)

    guard case .expandMacroResult(let expandedSource, let diagnostics) = response else {
      Issue.record("expected expandMacroResult, got \(response)")
      return
    }
    #expect(expandedSource == nil)
    #expect(diagnostics.map(\.message) == [accessorSkewMessage])
    #expect(!mock.receivedExpand)
  }

  // No over-rejection: a protocol-7 guest still expands a non-accessor macro.
  @Test
  func protocol7GuestExpandsNonAccessorMacro() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: 7)
    let (handler, loadResult) = handlerLoadingMock(mock)
    try #require(loadResult.isSuccessfulLoad)

    let response = handler.handleMessage(.freestandingExpansion(moduleName: moduleName, syntaxKind: .expression))

    guard case .expandMacroResult(let expandedSource, let diagnostics) = response else {
      Issue.record("expected expandMacroResult, got \(response)")
      return
    }
    #expect(expandedSource == "FORWARDED")
    #expect(diagnostics.isEmpty)
    #expect(mock.receivedExpand)
  }

  // Positive control: a protocol-8 guest receives accessor traffic without a diagnostic.
  @Test
  func protocol8GuestForwardsAccessorMacro() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: 8)
    let (handler, loadResult) = handlerLoadingMock(mock)
    try #require(loadResult.isSuccessfulLoad)

    let response = handler.handleMessage(.attachedExpansion(moduleName: moduleName, declKind: .accessor))

    guard case .expandMacroResult(let expandedSource, let diagnostics) = response else {
      Issue.record("expected expandMacroResult, got \(response)")
      return
    }
    #expect(expandedSource == "FORWARDED")
    #expect(diagnostics.isEmpty)
    #expect(mock.receivedExpand)
  }

  // A load with no preceding `getCapability` still forwards a (synthesized) capability and succeeds.
  @Test
  func nilHostCapabilityStillForwards() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
    let (_, loadResult) = handlerLoadingMock(mock, sendGetCapability: false)

    #expect(mock.forwardedProtocolVersions == [PluginMessage.PROTOCOL_VERSION_NUMBER])
    guard case .loadPluginLibraryResult(let loaded, _) = loadResult else {
      Issue.record("expected loadPluginLibraryResult, got \(loadResult)")
      return
    }
    #expect(loaded)
  }

  // A guest that cannot answer `getCapability` fails to load.
  @Test
  func guestFailingCapabilityHandshakeFailsLoad() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: 7, answersCapability: false)
    let (_, loadResult) = handlerLoadingMock(mock)

    guard case .loadPluginLibraryResult(let loaded, _) = loadResult else {
      Issue.record("expected loadPluginLibraryResult, got \(loadResult)")
      return
    }
    #expect(!loaded)
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
