//===--- ApkFile.swift - Shared libraries stored in an APK ---*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Android loads an app's shared libraries straight from its APK when they
//  are stored there uncompressed.  Such a library is mapped from the APK at
//  the offset of its entry, and its path is written `<apk>!/<entry>`, which
//  is the form the Android dynamic linker uses.
//
//===----------------------------------------------------------------------===//

#if os(Android)

import Swift

/// The uncompressed entries of an APK (a ZIP file).
@available(BacktracingDT 6.2, *)
struct ApkFile {
  struct Entry {
    var name: String
    var range: Range<Int>
  }

  /// The APK's contents, or `nil` if it couldn't be opened.
  let source: ImageSource?

  /// The uncompressed entries; empty if the APK couldn't be read.
  let entries: [Entry]

  /// Split an `<apk>!/<entry>` path into the APK path and the entry name.
  static func split(path: String) -> (apk: String, entry: String)? {
    let utf8 = path.utf8
    guard let bang = utf8.indices.first(where: {
      utf8[$0...].starts(with: "!/".utf8)
    }) else {
      return nil
    }
    return (String(path[..<bang]),
            String(path[utf8.index(bang, offsetBy: 2)...]))
  }

  init(path: String) {
    source = try? ImageSource(path: path)
    entries = source.flatMap(Self.readEntries) ?? []
  }

  private static func readEntries(_ source: ImageSource) -> [Entry]? {
    func u16(_ at: Int) throws -> Int {
      return Int(UInt16(littleEndian: try source.fetch(from: UInt64(at),
                                                       as: UInt16.self)))
    }
    func u32(_ at: Int) throws -> Int {
      return Int(UInt32(littleEndian: try source.fetch(from: UInt64(at),
                                                       as: UInt32.self)))
    }

    // The end of central directory record is followed by a comment of up
    // to 64KiB, so search backwards for its signature.
    let last = source.count - 22
    guard last >= 0, let eocd = stride(from: last,
                                       through: Swift.max(0, last - 0xffff),
                                       by: -1).first(where: {
      (try? u32($0)) == 0x06054b50
    }) else {
      return nil
    }

    var entries: [Entry] = []
    do {
      let count = try u16(eocd + 10)
      var header = try u32(eocd + 16)

      for _ in 0..<count {
        guard try u32(header) == 0x02014b50 else {
          return nil
        }
        let method = try u16(header + 10)
        let size = try u32(header + 24)
        let nameLength = try u16(header + 28)
        let extraLength = try u16(header + 30)
        let commentLength = try u16(header + 32)
        let localHeader = try u32(header + 42)

        // Only uncompressed entries can be mapped.
        if method == 0 {
          guard let name = try source.fetchString(
                  from: UInt64(header + 46), length: nameLength),
                try u32(localHeader) == 0x04034b50 else {
            return nil
          }
          let offset = localHeader + 30
            + (try u16(localHeader + 26)) + (try u16(localHeader + 28))
          entries.append(Entry(name: name, range: offset..<offset + size))
        }

        header += 46 + nameLength + extraLength + commentLength
      }
    } catch {
      return nil
    }
    return entries
  }

  /// Find the entry whose data contains the given offset in the APK.
  func entry(containing offset: Int) -> Entry? {
    return entries.first { $0.range.contains(offset) }
  }

  /// Find the entry with the given name.
  func entry(named name: String) -> Entry? {
    return entries.first { $0.name == name }
  }
}

#endif // os(Android)
