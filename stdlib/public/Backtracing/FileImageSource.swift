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

enum FileImageSourceError: Error {
  case posixError(Int32)
  case outOfRangeRead
}

class FileImageSource: ImageSource {
  private var _fd: Int32
  private var _mapping: UnsafeRawBufferPointer

  public var isMappedImage: Bool { return false }

  private var _path: String
  public var path: String? { return _path }

  public var bounds: Bounds? {
    return Bounds(base: 0, size: Size(_mapping.count))
  }

  public init(path: String) throws {
    _path = path
    _fd = _swift_open(path, O_RDONLY, 0)
    if _fd < 0 {
      throw FileImageSourceError.posixError(_swift_get_errno())
    }
    let size = lseek(_fd, 0, SEEK_END)
    if size < 0 {
      throw FileImageSourceError.posixError(_swift_get_errno())
    }
    let base = mmap(nil, Int(size), PROT_READ, MAP_FILE|MAP_PRIVATE, _fd, 0)
    if base == nil || base! == UnsafeRawPointer(bitPattern: -1)! {
      throw FileImageSourceError.posixError(_swift_get_errno())
    }
    _mapping = UnsafeRawBufferPointer(start: base, count: Int(size))
  }

  deinit {
    munmap(UnsafeMutableRawPointer(mutating: _mapping.baseAddress),
           _mapping.count)
    close(_fd)
  }

  public func fetch<T>(from addr: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    let size = buffer.count * MemoryLayout<T>.stride
    guard Int(addr) < _mapping.count && _mapping.count - Int(addr) >= size else {
      throw FileImageSourceError.outOfRangeRead
    }
    let slice = _mapping[Int(addr)...]
    _ = buffer.withMemoryRebound(to: UInt8.self) { byteBuf in
      byteBuf.update(from: slice)
    }
  }
}
