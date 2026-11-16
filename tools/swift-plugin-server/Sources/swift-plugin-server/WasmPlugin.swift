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

protocol WasmPlugin {
  func handleMessage(_ json: [UInt8]) throws -> [UInt8]

  func shutDown() throws
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
