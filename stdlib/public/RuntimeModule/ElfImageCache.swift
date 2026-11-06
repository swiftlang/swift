//===--- ElfImageCache.swift - ELF support for Swift ----------------------===//
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
// Provides a per-thread Elf image cache that improves efficiency when
// taking multiple backtraces by avoiding loading ELF images multiple times.
//
//===----------------------------------------------------------------------===//

import Swift

/// Provides a per-thread image cache for ELF image processing.  This means
/// if you take multiple backtraces from a thread, you won't load the same
/// image multiple times.
@available(BacktracingDT 6.2, *)
final class ElfImageCache {
  var elf32: [String: Elf32Image] = [:]
  var elf64: [String: Elf64Image] = [:]
  #if os(Android)
  var apks: [String: ApkFile] = [:]
  #endif

  func purge() {
    elf32 = [:]
    elf64 = [:]
    #if os(Android)
    apks = [:]
    #endif
  }

  #if os(Android)
  /// Get the APK at the given path, reading it only once.
  func apk(path: String) -> ApkFile {
    if let apk = apks[path] {
      return apk
    }
    let apk = ApkFile(path: path)
    apks[path] = apk
    return apk
  }
  #endif

  enum Result {
    case elf32Image(Elf32Image)
    case elf64Image(Elf64Image)
  }
  func lookup(path: String?) -> Result? {
    guard let path = path else {
      return nil
    }
    if let image = elf32[path] {
      return .elf32Image(image)
    }
    if let image = elf64[path] {
      return .elf64Image(image)
    }
    if let source = try? openSource(path: path) {
      if let elfImage = try? Elf32Image(source: source) {
        elf32[path] = elfImage
        return .elf32Image(elfImage)
      }
      if let elfImage = try? Elf64Image(source: source) {
        elf64[path] = elfImage
        return .elf64Image(elfImage)
      }
    }
    return nil
  }

  private func openSource(path: String) throws -> ImageSource {
    #if os(Android)
    // A library stored in an APK has a path of the form `<apk>!/<entry>`.
    if let (apkPath, entryName) = ApkFile.split(path: path) {
      let apk = apk(path: apkPath)
      guard let source = apk.source,
            let entry = apk.entry(named: entryName) else {
        throw ImageSourceError.outOfBoundsRead
      }
      return try source.subrange(UInt64(entry.range.lowerBound)
                                   ..< UInt64(entry.range.upperBound),
                                 path: path)
    }
    #endif
    return try ImageSource(path: path)
  }

  static var threadLocal: ElfImageCache {
    return BacktracerThreadLocals.threadLocal.elfImageCache
  }
}
