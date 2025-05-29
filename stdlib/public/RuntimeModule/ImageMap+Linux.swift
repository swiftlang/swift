//===--- ImageMap+Linux.swift --------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Linux specifics for ImageMap capture.
//
//===----------------------------------------------------------------------===//

#if os(Linux)

import Swift

#if canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

internal import BacktracingImpl.ImageFormats.Elf

fileprivate func readOSRelease(fd: CInt) -> [String:String]? {
  let len = lseek(fd, 0, SEEK_END)
  guard len >= 0 else {
    return nil
  }
  return withUnsafeTemporaryAllocation(byteCount: len, alignment: 16) {
    (buffer: UnsafeMutableRawBufferPointer) -> [String:String]? in

    _ = lseek(fd, 0, SEEK_SET)
    let bytesRead = read(fd, buffer.baseAddress, buffer.count)
    guard bytesRead == buffer.count else {
      return nil
    }

    let asString = String(decoding: buffer, as: UTF8.self)
    return Dictionary(OSReleaseScanner(asString),
                      uniquingKeysWith: { $1 })
  }
}

fileprivate func readOSRelease() -> [String:String]? {
  var fd = open("/etc/os-release", O_RDONLY)
  if fd == -1 {
    fd = open("/usr/lib/os-release", O_RDONLY)
  }
  if fd == -1 {
    return nil
  }
  defer {
    close(fd)
  }

  return readOSRelease(fd: fd)
}

extension ImageMap {

  private static var platform = {
    guard let info = readOSRelease(),
          let pretty = info["PRETTY_NAME"] else {
      return "Linux (unknown)"
    }

    return "Linux (\(pretty))"
  }()

  private struct AddressRange {
    var low: Address = 0
    var high: Address = 0
  }

  @_specialize(exported: true, kind: full, where M == UnsafeLocalMemoryReader)
  @_specialize(exported: true, kind: full, where M == RemoteMemoryReader)
  @_specialize(exported: true, kind: full, where M == LocalMemoryReader)
  @_spi(Internal)
  public static func capture<M: MemoryReader>(
    using reader: M,
    forProcess pid: Int? = nil
  ) -> ImageMap {
    var images: [Image] = []

    let wordSize: WordSize

    #if arch(x86_64) || arch(arm64) || arch(arm64_32)
    wordSize = .sixtyFourBit
    #elseif arch(i386) || arch(arm)
    wordSize = .thirtyTwoBit
    #endif

    let path: String
    if let pid = pid {
      path = "/proc/\(pid)/maps"
    } else {
      path = "/proc/self/maps"
    }

    guard let procMaps = readString(from: path) else {
      return ImageMap(platform: ImageMap.platform, images: [], wordSize: wordSize)
    }

    // Find all the mapped files and get high/low ranges
    var mappedFiles: [Substring:AddressRange] = [:]
    for match in ProcMapsScanner(procMaps) {
      let path = stripWhitespace(match.pathname)
      if match.inode == "0" || path == "" {
        continue
      }
      guard let start = Address(match.start, radix: 16),
            let end = Address(match.end, radix: 16) else {
        continue
      }

      if let range = mappedFiles[path] {
        mappedFiles[path] = AddressRange(low: Swift.min(start, range.low),
                                         high: Swift.max(end, range.high))
      } else {
        mappedFiles[path] = AddressRange(low: start,
                                         high: end)
      }
    }

    // Look at each mapped file to see if it's an ELF image
    for (path, range) in mappedFiles {
      // Extract the filename from path
      let name: Substring
      if let slashIndex = path.lastIndex(of: "/") {
        name = path.suffix(from: path.index(after: slashIndex))
      } else {
        name = path
      }

      // Inspect the image and extract the UUID and end of text
      guard let (endOfText, uuid) = getElfImageInfo(
              at: M.Address(exactly: range.low)!,
              using: reader
            ) else {
        // Not an ELF image
        continue
      }

      let image = Image(name: String(name),
                        path: String(path),
                        uniqueID: uuid,
                        baseAddress: range.low,
                        endOfText: Address(endOfText))

      images.append(image)
    }

    images.sort(by: { $0.baseAddress < $1.baseAddress })

    return ImageMap(
      platform: ImageMap.platform,
      images: images,
      wordSize: wordSize
    )
  }

}

#endif // os(Linux)
