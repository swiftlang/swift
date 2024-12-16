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

  init(path: FilePath, imports: WASIBridgeToHost) throws {
    var configuration = EngineConfiguration()
    configuration.stackSize = 1 << 20
    self.engine = Engine(configuration: configuration)
    let store = Store(engine: engine)

    let module = try parseWasm(filePath: path)
    var moduleImports = Imports()
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

  func shutDown() throws {
    // No resources requiring explicit shut down in `WasmKitEngine`.
  }
}
