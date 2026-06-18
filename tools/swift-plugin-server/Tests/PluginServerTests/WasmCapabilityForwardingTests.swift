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
import Testing
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

/// A base handler that answers `getCapability` with the server's version and
/// produces a benign result for anything else, so the intercepting handler is
/// exercised in isolation.
private final class NoopBaseHandler: PluginMessageHandler {
  func handleMessage(_ message: HostToPluginMessage) -> PluginToHostMessage {
    switch message {
    case .getCapability:
      return .getCapabilityResult(
        capability: PluginMessage.PluginCapability(
          protocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER,
          features: ["load-plugin-library"]
        )
      )
    default:
      return .expandMacroResult(expandedSource: nil, diagnostics: [])
    }
  }
  func shutDown() throws {}
}

private func makeSyntax(kind: PluginMessage.Syntax.Kind, source: String) -> PluginMessage.Syntax {
  PluginMessage.Syntax(
    kind: kind,
    source: source,
    location: PluginMessage.SourceLocation(
      fileID: "MyApp/test.swift", fileName: "test.swift", offset: 0, line: 1, column: 1
    )
  )
}

private let macroRef = PluginMessage.MacroReference(
  moduleName: "MacroDefinition", typeName: "ConstMacro", name: "constInt"
)

@Suite
struct WasmCapabilityForwardingTests {
  /// Builds a handler injected with `mock`, then drives the real handshake:
  /// optional host `getCapability`, then `loadPluginLibrary` for the `.wasm` guest.
  private func handlerLoadingMock(
    _ mock: MockWasmPlugin,
    sendGetCapability: Bool = true,
    hostProtocolVersion: Int = PluginMessage.PROTOCOL_VERSION_NUMBER
  ) -> (WasmInterceptingMessageHandler<NoopBaseHandler>, PluginToHostMessage) {
    let handler = WasmInterceptingMessageHandler(
      base: NoopBaseHandler(),
      makePlugin: { _ in mock }
    )
    if sendGetCapability {
      _ = handler.handleMessage(
        .getCapability(capability: PluginMessage.HostCapability(protocolVersion: hostProtocolVersion))
      )
    }
    let loadResult = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: macroRef.moduleName)
    )
    return (handler, loadResult)
  }

  private func attachedAccessorMessage(declKind: PluginMessage.Syntax.Kind) -> HostToPluginMessage {
    .expandAttachedMacro(
      macro: macroRef,
      macroRole: .accessor,
      discriminator: "$disc",
      attributeSyntax: makeSyntax(kind: .attribute, source: "@Foo"),
      declSyntax: makeSyntax(kind: declKind, source: "var x: Int"),
      parentDeclSyntax: nil,
      extendedTypeSyntax: nil,
      conformanceListSyntax: nil
    )
  }

  private func freestandingMessage(
    syntaxKind: PluginMessage.Syntax.Kind,
    lexicalContext: [PluginMessage.Syntax]? = nil
  ) -> HostToPluginMessage {
    .expandFreestandingMacro(
      macro: macroRef,
      macroRole: .expression,
      discriminator: "$disc",
      syntax: makeSyntax(kind: syntaxKind, source: "#constInt"),
      lexicalContext: lexicalContext
    )
  }

  // The guest must receive a forwarded `getCapability` (carrying the host version) on load.
  @Test
  func forwardsHostCapabilityToGuestOnLoad() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
    _ = handlerLoadingMock(mock)

    #expect(mock.receivedMessages.count == 1)
    #expect(mock.receivedForwardedCapability)
    if case .getCapability = mock.receivedMessages.first {} else {
      Issue.record("first guest message should be getCapability, got \(String(describing: mock.receivedMessages.first))")
    }
  }

  // A protocol-7 guest receiving an `.accessor` declSyntax gets a clear skew diagnostic, not the forward.
  @Test
  func diagnosesAccessorSkew_inDeclSyntax() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: 7)
    let (handler, _) = handlerLoadingMock(mock)

    let response = handler.handleMessage(attachedAccessorMessage(declKind: .accessor))

    guard case .expandMacroResult(let expandedSource, let diagnostics) = response else {
      Issue.record("expected expandMacroResult, got \(response)")
      return
    }
    #expect(expandedSource == nil)
    #expect(diagnostics.count == 1)
    let message = diagnostics.first?.message ?? ""
    #expect(message.contains("protocol version 7"))
    #expect(message.contains("protocol version \(PluginMessage.PROTOCOL_VERSION_NUMBER)"))
    #expect(message.contains("rebuild"))
    #expect(!mock.receivedExpand)
  }

  // `.accessor` in lexicalContext, not just the root syntax, must also be caught.
  @Test
  func diagnosesAccessorSkew_inLexicalContext() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: 7)
    let (handler, _) = handlerLoadingMock(mock)

    let response = handler.handleMessage(
      freestandingMessage(
        syntaxKind: .expression,
        lexicalContext: [makeSyntax(kind: .accessor, source: "get")]
      )
    )

    guard case .expandMacroResult(let expandedSource, let diagnostics) = response else {
      Issue.record("expected expandMacroResult, got \(response)")
      return
    }
    #expect(expandedSource == nil)
    #expect(diagnostics.count == 1)
    #expect((diagnostics.first?.message ?? "").contains("rebuild"))
    #expect(!mock.receivedExpand)
  }

  // No over-rejection: a protocol-7 guest still expands a non-accessor macro.
  @Test
  func protocol7GuestExpandsNonAccessorMacro() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: 7)
    let (handler, _) = handlerLoadingMock(mock)

    let response = handler.handleMessage(freestandingMessage(syntaxKind: .expression))

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
    let (handler, _) = handlerLoadingMock(mock)

    let response = handler.handleMessage(attachedAccessorMessage(declKind: .accessor))

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

    #expect(mock.receivedForwardedCapability)
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
