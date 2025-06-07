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

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
internal import Darwin
#elseif os(Windows)
internal import ucrt
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

/// Provides a per-thread image cache for ELF image processing.  This means
/// if you take multiple backtraces from a thread, you won't load the same
/// image multiple times.
final class ElfImageCache {
  var elf32: [String: Elf32Image] = [:]
  var elf64: [String: Elf64Image] = [:]

  func purge() {
    elf32 = [:]
    elf64 = [:]
  }

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
    if let source = try? ImageSource(path: path) {
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

  private static var key: pthread_key_t = {
    var theKey = pthread_key_t()
    let err = pthread_key_create(
      &theKey,
      { rawPtr in
        let ptr = Unmanaged<ElfImageCache>.fromOpaque(
          notMutable(notOptional(rawPtr))
        )
        ptr.release()
      }
    )
    if err != 0 {
      fatalError("Unable to create TSD key for ElfImageCache")
    }
    return theKey
  }()

  static var threadLocal: ElfImageCache {
    guard let rawPtr = pthread_getspecific(key) else {
      let cache = Unmanaged<ElfImageCache>.passRetained(ElfImageCache())
      pthread_setspecific(key, cache.toOpaque())
      return cache.takeUnretainedValue()
    }
    let cache = Unmanaged<ElfImageCache>.fromOpaque(rawPtr)
    return cache.takeUnretainedValue()
  }
}
