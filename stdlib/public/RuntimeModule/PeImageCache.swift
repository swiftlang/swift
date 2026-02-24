//===--- PeImageCache.swift - PE-COFF support for Swift -------------------===//
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
// Provides a per-thread PE-COFF image cache that improves efficiency when
// taking multiple backtraces by avoiding loading PE-COFF images multiple times.
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
internal import Darwin
#elseif os(Windows)
internal import WinSDK
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

/// Provides a per-thread image cache for PE-COFF image processing.  This means
/// if you take multiple backtraces from a thread, you won't load the same
/// image multiple times.
final class PeImageCache {
  var cache: [String: PeCoffImage] = [:]

  func purge() {
    cache = [:]
  }

  func lookup(path: String?) -> PeCoffImage? {
    guard let path = path else {
      return nil
    }
    if let image = cache[path] {
      return image
    }
    if let source = try? ImageSource(path: path),
       let image = try? PeCoffImage(source: source) {
      cache[path] = image
      return image
    }
    return nil
  }

  #if os(Windows)
  private static var dwTlsIndex: DWORD = {
    let dwNdx = TlsAlloc()
    if dwNdx == TLS_OUT_OF_INDEXES {
      fatalError("Unable to allocate TSD for PeImageCache")
    }
    return dwNdx
  }()
  #else
  private static var key: pthread_key_t = {
    var theKey = pthread_key_t()
    let err = pthread_key_create(
      &theKey,
      { rawPtr in
        let ptr = Unmanaged<PeImageCache>.fromOpaque(
          notMutable(notOptional(rawPtr))
        )
        ptr.release()
      }
    )
    if err != 0 {
      fatalError("Unable to create TSD key for PeImageCache")
    }
    return theKey
  }()
  #endif

  static var threadLocal: PeImageCache {
    #if os(Windows)
    guard let rawPtr = TlsGetValue(dwTlsIndex) else {
      let cache = Unmanaged<PeImageCache>.passRetained(PeImageCache())
      TlsSetValue(dwTlsIndex, cache.toOpaque())
      return cache.takeUnretainedValue()
    }
    #else
    guard let rawPtr = pthread_getspecific(key) else {
      let cache = Unmanaged<PeImageCache>.passRetained(PeImageCache())
      pthread_setspecific(key, cache.toOpaque())
      return cache.takeUnretainedValue()
    }
    #endif
    let cache = Unmanaged<PeImageCache>.fromOpaque(rawPtr)
    return cache.takeUnretainedValue()
  }
}
