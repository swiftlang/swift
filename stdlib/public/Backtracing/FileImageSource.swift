//===--- FileImageSource.swift - An image source that reads from a file ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines FileImageSource, an image source that reads data from a file.
//
//===----------------------------------------------------------------------===//

import Swift

@_implementationOnly import OS.Libc

private enum FileImageSourceError: Error {
  case posixError(Int32)
  case truncatedRead
}

private class FileImageSource: ImageSource {
  typealias Address = UInt64
  typealias Size = UInt64

  private var fd: Int32

  public var isMappedImage: Bool { return false }

  public init?(path: String) {
    fd = _swift_open(path, O_RDONLY, 0)
  }

  deinit {
    close(fd)
  }

  public func fetch<T>(from addr: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    while true {
      let size = MemoryLayout<T>.stride * buffer.count
      let result = pread(fd, buffer.baseAddress, size, off_t(addr))

      if result < 0 {
        throw FileImageSourceError.posixError(_swift_get_errno())
      }

      if result != size {
        throw FileImageSourceError.truncatedRead
      }
      break
    }
  }
}
