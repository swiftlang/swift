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

#if SWIFT_PLUGIN_SERVER_SUPPORTS_WASM

import SystemPackage
@_spi(WASIPlatform) import WASI
import WasmKit
import WasmKitWASI

typealias DefaultWasmEngine = WasmKitEngine

struct WasmKitEngine: WasmEngine {
  private let engine: Engine
  private let functions: [String: Function]
  private let stdin: WasmStdioBuffer
  private let stdout: WasmStdioBuffer
  private let wasi: WASIBridgeToHost

  init(pluginPath: FilePath) throws {
    let module = try parseWasm(filePath: pluginPath.string)
    let stdin = WasmStdioBuffer(accessMode: .read)
    let stdout = WasmStdioBuffer(accessMode: .write)
    let wasi = try WASIBridgeToHost(
      fileSystem: .host().withStdio(stdin: stdin, stdout: stdout, stderr: WasmStandardError())
    )
    do {
      var configuration = EngineConfiguration()
      configuration.stackSize = 1 << 20
      self.engine = Engine(configuration: configuration)
      let store = Store(engine: engine)
      var moduleImports = Imports()
      wasi.link(to: &moduleImports, store: store)
      let instance = try module.instantiate(store: store, imports: moduleImports)
      var functions = [String: Function]()
      for (name, export) in instance.exports {
        guard case let .function(function) = export else { continue }
        functions[name] = function
      }
      self.functions = functions
    } catch {
      try Cleanup.throwAfterRunning(wasi.close, error: error)
    }
    self.stdin = stdin
    self.stdout = stdout
    self.wasi = wasi
  }

  func function(named name: String) throws -> WasmFunction? {
    guard let function = functions[name] else { return nil }
    return { _ = try function.invoke() }
  }

  func writeToPlugin(_ storage: some Sequence<UInt8>) throws {
    self.stdin.append(storage)
  }

  func readFromPlugin(into storage: UnsafeMutableRawBufferPointer) throws -> Int {
    self.stdout.take(into: storage)
  }

  func shutDown() throws {
    try self.wasi.close()
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
