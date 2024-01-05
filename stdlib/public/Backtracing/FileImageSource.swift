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
  private var _mapping: UnsafeRawBufferPointer

  public var isMappedImage: Bool { return false }

  private var _path: String
  public var path: String? { return _path }

  public var bounds: Bounds? {
    return Bounds(base: 0, size: Size(_mapping.count))
  }

  public init(path: String) throws {
    _path = path
    let fd = _swift_open(path, O_RDONLY, 0)
    if fd < 0 {
      throw FileImageSourceError.posixError(_swift_get_errno())
    }
    defer { close(fd) }
    let size = lseek(fd, 0, SEEK_END)
    if size < 0 {
      throw FileImageSourceError.posixError(_swift_get_errno())
    }
    let base = mmap(nil, Int(size), PROT_READ, MAP_FILE|MAP_PRIVATE, fd, 0)
    if base == nil || base! == UnsafeRawPointer(bitPattern: -1)! {
      throw FileImageSourceError.posixError(_swift_get_errno())
    }
    _mapping = UnsafeRawBufferPointer(start: base, count: Int(size))
  }

  deinit {
    munmap(UnsafeMutableRawPointer(mutating: _mapping.baseAddress),
           _mapping.count)
  }

  public func fetch(from addr: Address,
                    into buffer: UnsafeMutableRawBufferPointer) throws {
    let start = Int(addr)
    guard _mapping.indices.contains(start) else {
      throw FileImageSourceError.outOfRangeRead
    }
    let slice = _mapping[start...]
    guard slice.count >= buffer.count else {
      throw FileImageSourceError.outOfRangeRead
    }
    buffer.copyBytes(from: slice[start..<start+buffer.count])
  }
}
