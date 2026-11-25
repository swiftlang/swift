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

import SystemPackage
@_spi(WASIPlatform) import WASI

/// Forwards the guest's standard error to the plugin server's.
struct WasmStandardError: WasmStdioStream {
  var accessMode: FileAccessMode { .write }

  func read(into buffers: GuestBuffers) throws -> WASIAbi.Size {
    throw WASIAbi.Errno.EBADF
  }

  func write(vectored buffers: GuestBuffers) throws -> WASIAbi.Size {
    var total: WASIAbi.Size = 0
    for index in 0..<buffers.count {
      total += WASIAbi.Size(
        try buffers.withHostBuffer(at: index) { buffer in
          do {
            return try FileDescriptor.standardError.writeAll(UnsafeRawBufferPointer(buffer))
          } catch {
            throw WASIAbi.Errno.EIO
          }
        })
    }
    return total
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
