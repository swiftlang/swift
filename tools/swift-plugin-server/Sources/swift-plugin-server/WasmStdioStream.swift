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

@_spi(WASIPlatform) import WASI

protocol WasmStdioStream: WASIFile {
  var accessMode: FileAccessMode { get }
}

extension WasmStdioStream {
  func attributes() throws -> WASIAbi.Filestat {
    WASIAbi.Filestat(dev: 0, ino: 0, filetype: .CHARACTER_DEVICE, nlink: 1, size: 0, atim: 0, mtim: 0, ctim: 0)
  }

  func fileType() throws -> WASIAbi.FileType { .CHARACTER_DEVICE }

  func status() throws -> WASIAbi.Fdflags { [] }

  func setTimes(atim: WASIAbi.Timestamp, mtim: WASIAbi.Timestamp, fstFlags: WASIAbi.FstFlags) throws {}

  func advise(offset: WASIAbi.FileSize, length: WASIAbi.FileSize, advice: WASIAbi.Advice) throws {}

  func close() throws {}

  func fdStat() throws -> WASIAbi.FdStat {
    var fsRightsBase: WASIAbi.Rights = []
    if accessMode.contains(.read) {
      fsRightsBase.insert(.FD_READ)
    }
    if accessMode.contains(.write) {
      fsRightsBase.insert(.FD_WRITE)
    }
    return WASIAbi.FdStat(
      fsFileType: .CHARACTER_DEVICE,
      fsFlags: [],
      fsRightsBase: fsRightsBase,
      fsRightsInheriting: []
    )
  }

  func setFdStatFlags(_ flags: WASIAbi.Fdflags) throws {}

  func setFilestatSize(_ size: WASIAbi.FileSize) throws { throw WASIAbi.Errno.EINVAL }

  func sync() throws {}

  func datasync() throws {}

  func tell() throws -> WASIAbi.FileSize { throw WASIAbi.Errno.ESPIPE }

  func seek(offset: WASIAbi.FileDelta, whence: WASIAbi.Whence) throws -> WASIAbi.FileSize {
    throw WASIAbi.Errno.ESPIPE
  }

  func pwrite(vectored buffers: GuestBuffers, offset: WASIAbi.FileSize) throws -> WASIAbi.Size {
    throw WASIAbi.Errno.ESPIPE
  }

  func pread(into buffers: GuestBuffers, offset: WASIAbi.FileSize) throws -> WASIAbi.Size {
    throw WASIAbi.Errno.ESPIPE
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
