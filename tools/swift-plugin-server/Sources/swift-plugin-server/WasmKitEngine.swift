//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SystemPackage
import WASI
import WasmKit
import WasmKitWASI

typealias DefaultWasmEngine = WasmKitEngine

struct WasmKitEngine: WasmEngine {
  private let engine: Engine
  private let functions: [String: Function]
  private let hostToPlugin: FileDescriptor
  private let pluginToHost: FileDescriptor

  init(pluginPath: FilePath) throws {
    let hostToPluginPipe = try FileDescriptor.pipe()
    let pluginToHostPipe = try FileDescriptor.pipe()
    self.hostToPlugin = hostToPluginPipe.writeEnd
    self.pluginToHost = pluginToHostPipe.readEnd

    var configuration = EngineConfiguration()
    configuration.stackSize = 1 << 20
    self.engine = Engine(configuration: configuration)
    let store = Store(engine: engine)

    let module = try parseWasm(filePath: pluginPath)
    var moduleImports = Imports()

    let imports = try WASIBridgeToHost(
      stdin: hostToPluginPipe.readEnd,
      stdout: pluginToHostPipe.writeEnd,
      stderr: .standardError
    )
    imports.link(to: &moduleImports, store: store)
    let instance = try module.instantiate(store: store, imports: moduleImports)
    var functions = [String: Function]()
    for (name, export) in instance.exports {
      guard case let .function(function) = export else { continue }
      functions[name] = function
    }

    self.functions = functions
  }

  func function(named name: String) throws -> WasmFunction? {
    guard let function = functions[name] else { return nil }
    return { _ = try function.invoke() }
  }

  func writeToPlugin(_ storage: some Sequence<UInt8>) throws {
    try self.hostToPlugin.writeAll(storage)
  }

  func readFromPlugin(into storage: UnsafeMutableRawBufferPointer) throws -> Int {
    try self.pluginToHost.read(into: storage)
  }

  func shutDown() throws {
    try self.hostToPlugin.close()
    try self.pluginToHost.close()
  }
}
