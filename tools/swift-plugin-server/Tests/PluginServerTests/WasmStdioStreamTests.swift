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
@_spi(WASIPlatform) import WASI

@Suite
struct WasmStdioStreamTests {
  @Test(arguments: [
    (FileAccessMode.read, WASIAbi.Rights.FD_READ),
    (FileAccessMode.write, WASIAbi.Rights.FD_WRITE),
    ([.read, .write] as FileAccessMode, [.FD_READ, .FD_WRITE] as WASIAbi.Rights),
  ])
  func fdStatGrantsAccessModeRights(_ accessMode: FileAccessMode, _ rights: WASIAbi.Rights) throws {
    #expect(try WasmStdioBuffer(accessMode: accessMode).fdStat().fsRightsBase == rights)
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
