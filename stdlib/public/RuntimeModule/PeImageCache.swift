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

  static var threadLocal: PeImageCache {
    return BacktracerThreadLocals.threadLocal.peImageCache
  }
}
