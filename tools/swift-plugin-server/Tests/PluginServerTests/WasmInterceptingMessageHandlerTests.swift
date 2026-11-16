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
import SystemPackage
import Testing
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

@Suite
struct WasmInterceptingMessageHandlerTests {
  @Test
  func shutDownReachesEveryPlugin() throws {
    let plugins = [
      "A": MockWasmPlugin(
        reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER,
        shutDownError: WasmEngineError(message: "A failed")
      ),
      "B": MockWasmPlugin(
        reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER,
        shutDownError: WasmEngineError(message: "B failed")
      ),
    ]
    let handler = WasmInterceptingMessageHandler(
      base: MockBaseHandler(),
      makePlugin: { path in plugins[path.stem!]! }
    )
    for moduleName in plugins.keys {
      try #require(
        handler.handleMessage(.loadPluginLibrary(libraryPath: "/tmp/\(moduleName).wasm", moduleName: moduleName))
          .isSuccessfulLoad
      )
    }

    let error = #expect(throws: WasmEngineError.self) { try handler.shutDown() }

    #expect(plugins["A"]!.shutDownCount == 1)
    #expect(plugins["B"]!.shutDownCount == 1)
    #expect(error?.description.contains("A failed") == true)
    #expect(error?.description.contains("B failed") == true)
  }

  @Test
  func shutDownReachesBaseHandler() throws {
    let base = MockBaseHandler()
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in
        MockWasmPlugin(
          reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER,
          shutDownError: WasmEngineError(message: "plugin failed")
        )
      }
    )
    try #require(
      handler.handleMessage(.loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition"))
        .isSuccessfulLoad
    )

    let error = #expect(throws: WasmEngineError.self) { try handler.shutDown() }

    #expect(error?.description == "plugin failed")
    #expect(base.shutDownCount == 1)
  }

  @Test
  func repeatedLoadReusesLoadedPlugin() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
    let base = MockBaseHandler()
    var madePlugins = 0
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in
        madePlugins += 1
        return mock
      }
    )
    let load = HostToPluginMessage.loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition")
    try #require(handler.handleMessage(load).isSuccessfulLoad)

    let response = handler.handleMessage(load)

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(loaded)
    #expect(diagnostics.isEmpty)
    #expect(madePlugins == 1)
    #expect(mock.shutDownCount == 0)
    #expect(base.receivedMessages.isEmpty)
  }

  @Test
  func loadFromSecondPathIsRejected() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
    let base = MockBaseHandler()
    var madePlugins = 0
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in
        madePlugins += 1
        return mock
      }
    )
    try #require(
      handler.handleMessage(.loadPluginLibrary(libraryPath: "/tmp/A.wasm", moduleName: "MacroDefinition"))
        .isSuccessfulLoad
    )

    let response = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/B.wasm", moduleName: "MacroDefinition")
    )

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(!loaded)
    #expect(diagnostics.map(\.message) == [
      "Wasm plugin for module 'MacroDefinition' is already loaded from different path '/tmp/A.wasm'"
    ])
    #expect(madePlugins == 1)
    #expect(mock.shutDownCount == 0)
    #expect(base.receivedMessages.isEmpty)
  }

  @Test
  func samePathGetsPluginPerModule() throws {
    let base = MockBaseHandler()
    var plugins: [MockWasmPlugin] = []
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in
        let plugin = MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
        plugins.append(plugin)
        return plugin
      }
    )
    try #require(
      handler.handleMessage(.loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "A")).isSuccessfulLoad
    )

    let response = handler.handleMessage(.loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "B"))

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(loaded)
    #expect(diagnostics.isEmpty)
    _ = handler.handleMessage(.freestandingExpansion(moduleName: "A"))
    _ = handler.handleMessage(.freestandingExpansion(moduleName: "B"))
    #expect(plugins.map(\.expandedModuleNames) == [["A"], ["B"]])
    #expect(base.receivedMessages.isEmpty)
  }

  @Test
  func nonWasmLoadReachesBaseHandler() throws {
    let base = MockBaseHandler()
    var madePlugins = 0
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in
        madePlugins += 1
        return MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
      }
    )

    let response = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/libMacroDefinition.dylib", moduleName: "MacroDefinition")
    )

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(loaded)
    #expect(diagnostics.isEmpty)
    #expect(madePlugins == 0)
    #expect(base.receivedMessages.count == 1)
    guard case .loadPluginLibrary("/tmp/libMacroDefinition.dylib", "MacroDefinition") = base.receivedMessages.first else {
      Issue.record("base should receive the load, got \(String(describing: base.receivedMessages.first))")
      return
    }
  }

  @Test
  func libraryLoadOfLoadedWasmModuleIsRejected() throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
    let base = MockBaseHandler()
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in mock }
    )
    try #require(
      handler.handleMessage(.loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition"))
        .isSuccessfulLoad
    )

    let response = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/libMacroDefinition.dylib", moduleName: "MacroDefinition")
    )

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(!loaded)
    #expect(diagnostics.map(\.message) == [
      "Wasm plugin for module 'MacroDefinition' is already loaded from different path '/tmp/Plugin.wasm'"
    ])
    #expect(base.receivedMessages.isEmpty)
    _ = handler.handleMessage(.freestandingExpansion(moduleName: "MacroDefinition"))
    #expect(mock.expandedModuleNames == ["MacroDefinition"])
  }

  @Test
  func wasmLoadOfLoadedLibraryModuleIsRejected() throws {
    let base = MockBaseHandler()
    var madePlugins = 0
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in
        madePlugins += 1
        return MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
      }
    )
    let libraryLoad = HostToPluginMessage.loadPluginLibrary(
      libraryPath: "/tmp/libMacroDefinition.dylib", moduleName: "MacroDefinition"
    )
    try #require(handler.handleMessage(libraryLoad).isSuccessfulLoad)

    let response = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition")
    )

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(!loaded)
    #expect(diagnostics.map(\.message) == [
      """
      library plugin for module 'MacroDefinition' is already loaded from different path \
      '/tmp/libMacroDefinition.dylib'
      """
    ])
    #expect(madePlugins == 0)
    let expansion = HostToPluginMessage.freestandingExpansion(moduleName: "MacroDefinition")
    _ = handler.handleMessage(expansion)
    #expect(try base.receivedMessages.map { try JSON.encode($0) } == [JSON.encode(libraryLoad), JSON.encode(expansion)])
  }

  @Test
  func wasmLoadAfterFailedLibraryLoadSucceeds() throws {
    let handler = WasmInterceptingMessageHandler(
      base: MockBaseHandler(failsLoads: true),
      makePlugin: { _ in MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER) }
    )
    let libraryResponse = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/libMacroDefinition.dylib", moduleName: "MacroDefinition")
    )
    try #require(!libraryResponse.isSuccessfulLoad)

    let response = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition")
    )

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(loaded)
    #expect(diagnostics.isEmpty)
  }

  @Test(arguments: [false, true])
  func nonWasmExpansionReachesBaseHandler(attached: Bool) throws {
    let mock = MockWasmPlugin(reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
    let base = MockBaseHandler()
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in mock }
    )
    try #require(
      handler.handleMessage(.loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition"))
        .isSuccessfulLoad
    )
    let request: HostToPluginMessage = attached
      ? .attachedExpansion(moduleName: "NativeMacros", declKind: .declaration)
      : .freestandingExpansion(moduleName: "NativeMacros")

    let response = handler.handleMessage(request)

    guard case .expandMacroResult(let expandedSource, let diagnostics) = response else {
      Issue.record("expected expandMacroResult, got \(response)")
      return
    }
    #expect(expandedSource == nil)
    #expect(diagnostics.isEmpty)
    #expect(!mock.receivedExpand)
    #expect(try base.receivedMessages.map { try JSON.encode($0) } == [JSON.encode(request)])
  }

  @Test
  func failedExpansionReportsPluginError() throws {
    let base = MockBaseHandler()
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in
        MockWasmPlugin(reportedProtocolVersion: 7, expandError: WasmEngineError(message: "guest trapped"))
      }
    )
    try #require(
      handler.handleMessage(.loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition"))
        .isSuccessfulLoad
    )

    let response = handler.handleMessage(.freestandingExpansion(moduleName: "MacroDefinition"))

    guard case .expandMacroResult(let expandedSource, let diagnostics) = response else {
      Issue.record("expected expandMacroResult, got \(response)")
      return
    }
    #expect(expandedSource == nil)
    #expect(diagnostics.map(\.message) == [
      """
      failed to communicate with external macro implementation type 'MacroDefinition.ConstMacro' to expand \
      macro 'constInt'; the plugin reports protocol version 7 and the host uses \
      \(PluginMessage.PROTOCOL_VERSION_NUMBER); guest trapped
      """
    ])
    #expect(diagnostics.map(\.severity) == [.error])
    #expect(diagnostics.map(\.position.fileName) == ["test.swift"])
    #expect(diagnostics.map(\.position.offset) == [0])
    #expect(base.receivedMessages.isEmpty)
  }

  @Test
  func failedPluginConstructionFailsLoad() throws {
    let base = MockBaseHandler()
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in throw WasmEngineError(message: "construction failed") }
    )

    let response = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition")
    )

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(!loaded)
    #expect(diagnostics.map(\.message) == ["construction failed"])
    #expect(base.receivedMessages.isEmpty)
  }

  @Test
  func failedHandshakeShutsDownPlugin() throws {
    let mock = MockWasmPlugin(
      reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER,
      answersCapability: false,
      shutDownError: WasmEngineError(message: "shutdown failed")
    )
    let base = MockBaseHandler()
    let handler = WasmInterceptingMessageHandler(
      base: base,
      makePlugin: { _ in mock }
    )

    let response = handler.handleMessage(
      .loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition")
    )

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(!loaded)
    #expect(diagnostics.map(\.message) == [
      "Wasm plugin did not answer getCapability with getCapabilityResult",
      "additionally, shutting down the failed Wasm plugin errored: shutdown failed",
    ])
    #expect(mock.shutDownCount == 1)
    #expect(base.receivedMessages.isEmpty)
  }

  @Test
  func loadAfterFailedHandshakeRebuildsPlugin() throws {
    var madePlugins = 0
    let handler = WasmInterceptingMessageHandler(
      base: MockBaseHandler(),
      makePlugin: { _ in
        madePlugins += 1
        return MockWasmPlugin(
          reportedProtocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER,
          answersCapability: madePlugins > 1
        )
      }
    )
    let load = HostToPluginMessage.loadPluginLibrary(libraryPath: "/tmp/Plugin.wasm", moduleName: "MacroDefinition")
    let firstResponse = handler.handleMessage(load)
    guard case .loadPluginLibraryResult(loaded: false, let firstDiagnostics) = firstResponse else {
      Issue.record("expected the first load to fail, got \(firstResponse)")
      return
    }
    #expect(firstDiagnostics.map(\.message) == ["Wasm plugin did not answer getCapability with getCapabilityResult"])

    let response = handler.handleMessage(load)

    guard case .loadPluginLibraryResult(let loaded, let diagnostics) = response else {
      Issue.record("expected loadPluginLibraryResult, got \(response)")
      return
    }
    #expect(loaded)
    #expect(diagnostics.isEmpty)
    #expect(madePlugins == 2)
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
