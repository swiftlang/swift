//===--- Compression.swift - Data compression for ELF images --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines the compressed image sources, which are used to access compressed
// data from ELF images.
//
// There are three different compression formats we might have to interact
// with, namely zlib (deflate), zstd and LZMA.  We don't implement the
// decompression algorithms here, but rather we will try to use zlib,
// zstd and liblzma respectively.
//
// We support two different modes; one is where the compression libraries
// have been statically linked with us, in which case we can just call them;
// the other is where we will try to load them with `dlopen()`.  We do this
// so as to avoid a hard dependency on them in the runtime.
//
//===----------------------------------------------------------------------===//

#if os(Linux)

import Swift

@_implementationOnly import Compression
@_implementationOnly import ImageFormats.Elf

enum CompressedImageSourceError: Error {
  case unboundedImageSource
  case outOfRangeFetch(Int, Int)
  case badCompressedData
  case unsupportedFormat
}

internal struct ElfCompressedImageSource<Traits: ElfTraits>: ImageSource {

  init(source: ImageSource) throws {
    guard let bounds = source.bounds else {
      throw CompressedImageSourceError.unboundedImageSource
    }

    if bounds.size < MemoryLayout<Traits.Chdr>.size {
      throw CompressedImageSourceError.badCompressedData
    }

    let chdr = try source.fetch(from: source.bounds.base,
                                as: Traits.Chdr.self)

    switch chdr.ch_type {
      case ELF_COMPRESS_ZLIB:
        // ###TODO
      case ELF_COMPRESS_ZSTD:
        // ###TODO
      default:
        throw CompressedImageSourceError.unsupportedFormat
    }
  }

}

internal struct ElfGNUCompressedImageSource: ImageSource {

  private var data: [UInt8]

  var isMappedImage: Bool { return false }
  var path: String? { return nil }
  var bounds: Bounds? { return Bounds(base: 0, size: data.count) }

  init(source: ImageSource) throws {
    guard let bounds = source.bounds else {
      throw CompressedImageSourceError.unboundedImageSource
    }

    if bounds.size < 12 {
      throw CompressedImageSourceError.badCompressedData
    }

    let magic = try source.fetch(from: bounds.base, as: UInt32.self)
    if magic != 0x42494c5a {
      throw CompressedImageSourceError.badCompressedData
    }

    let uncompressedLength
      = try source.fetch(from: bounds.base + 4, as: UInt64.self).byteSwapped

    let stream = try ZLibStream()

    var pos = bounds.base + 12
    var remaining = bounds.size - 12

    // ###FIXME: Could use uncompressedLength here

    data = [UInt8]()

    try stream.decode(
      from: {
        (inputBuffer: UnsafeMutableBufferPointer<UInt8>) throws -> UInt in

        let chunkSize = max(remaining, inputBuffer.count)
        let slice = inputBuffer[0..<chunkSize]
        let buffer = UnsafeMutableBufferPointer(rebasing: slice)

        try source.fetch(from: pos, into: buffer)

        pos += chunkSize
        remaining -= chunkSize

        return chunkSize
      },
      to: {
        (outputBuffer: UnsafeBufferPointer<UInt8>) in

        data.append(contentsOf: outputBuffer)
      }
    )
  }

  public func fetch<T>(from addr: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    let toFetch = buffer.count * MemoryLayout<T>.stride
    if addr < 0 || addr > data.count || data.count - addr < toFetch {
      throw CompressedImageSourceError.outOfRangeFetch(addr, toFetch)
    }

    buffer.withMemoryRebound(to: UInt8.self) { outBuf in
      for n in 0..<toFetch {
        outBuf[n] = data[addr + n]
      }
    }
  }

}

internal struct LZMACompressedImageSource: ImageSource {
  typealias Address = Int
  typealias Size = Int

  private var data: [UInt8]

  var isMappedImage: Bool { return false }
  var path: String? { return nil }
  var bounds: Bounds? { return Bounds(base: 0, size: data.count) }

  init(source: ImageSource) throws {
    // Only supported for bounded image sources
    guard let bounds = source.bounds else {
      throw CompressedImageSource.unboundedImageSource
    }

    // Decode the data into the array
    let stream = try LZMAStream()

    var pos = bounds.base
    var remaining = bounds.size

    data = [UInt8]()

    try stream.decode(
      from: {
        (inputBuffer: UnsafeMutableBufferPointer<UInt8>) throws -> UInt in

        let chunkSize = max(remaining, inputBuffer.count)
        let slice = inputBuffer[0..<chunkSize]
        let buffer = UnsafeMutableBufferPointer(rebasing: slice)

        try source.fetch(from: pos, into: buffer)

        pos += chunkSize
        remaining -= chunkSize

        return chunkSize
      },
      to: {
        (outputBuffer: UnsafeBufferPointer<UInt8>) in

        data.append(contentsOf: outputBuffer)
      }
    )
  }

  public func fetch<T>(from addr: Address,
                       into buffer: UnsafeMutableBufferPointer<T>) throws {
    let toFetch = buffer.count * MemoryLayout<T>.stride
    if addr < 0 || addr > data.count || data.count - addr < toFetch {
      throw CompressedImageSourceError.outOfRangeFetch(addr, toFetch)
    }

    buffer.withMemoryRebound(to: UInt8.self) { outBuf in
      for n in 0..<toFetch {
        outBuf[n] = data[addr + n]
      }
    }
  }

}

#endif // os(Linux)
