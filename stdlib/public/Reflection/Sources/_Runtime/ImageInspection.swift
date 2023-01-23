//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift
import _SwiftRuntimeShims

#if !(os(macOS) || os(iOS) || os(tvOS) || os(watchOS))
import SwiftShims
#endif

@available(SwiftStdlib 5.9, *)
@frozen
public enum ImageInspection {}

//===----------------------------------------------------------------------===//
// Mach-O Image Lookup
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
@_silgen_name("initializeDyldLookup")
func initializeDyldLookup()
#endif

//===----------------------------------------------------------------------===//
// ELF and COFF Image Lookup
//===----------------------------------------------------------------------===//

#if !(os(macOS) || os(iOS) || os(tvOS) || os(watchOS))
func enumerateSections<K: Hashable, V>(
  _ body: (MetadataSections) -> (
      MetadataSectionRange,
      (UnsafeRawPointer, Int) -> ()
    )
) {
  swift_enumerateAllMetadataSections({ sections, _ in
    guard let sections = sections else {
      return true
    }

    let metadataSections = sections.assumingMemoryBound(
      to: MetadataSections.self
    )

    guard metadataSections.version >= 3 else {
      return false
    }

    let (range, register) = body(metadataSections)

    register(range.start, range.length)

    return true
  }, nil)
}
#endif

//===----------------------------------------------------------------------===//
// Runtime Attributes
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
struct AttributeCache {
#if !(os(macOS) || os(iOS) || os(tvOS) || os(watchOS))
  var imageCount = 0
#endif
  var map: [ContextDescriptor: [UnsafeRawPointer]] = [:]
}

@available(SwiftStdlib 5.9, *)
var attributeCache: Lock<AttributeCache> = .create(with: .init())

@available(SwiftStdlib 5.9, *)
extension ImageInspection {
  @available(SwiftStdlib 5.9, *)
  public static func withAttributeCache<T>(
    _ body: @Sendable ([ContextDescriptor: [UnsafeRawPointer]]) throws -> T
  ) rethrows -> T {
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
    initializeDyldLookup()

    return try attributeCache.withLock {
      try body($0.map)
    }
#else
    return try attributeCache.withLock {
      // If our cached image count is less than what's being reported by the
      // Swift runtime, then we need to reset our cache and enumerate all of
      // the images again for updated attributes.
      let currentImageCount = swift_getMetadataSectionCount()

      if $0.imageCount < currentImageCount {
        $0.map.removeAll(keepingCapacity: true)

        // We enumerate inside the lock because we need to ensure that the
        // thread who gets this lock first is able to at least initialize the
        // cache before other threads.
        enumerateSections {
          ($0.swift5_runtime_attributes, registerAttributes(_:_:))
        }

        $0.imageCount = currentImageCount
      }

      return try body($0.map)
    }
#endif
  }
}

@available(SwiftStdlib 5.9, *)
@_cdecl("registerAttributes")
func registerAttributes(_ section: UnsafeRawPointer, _ size: Int) {
  var address = section
  let end = address + size

  while address < end {
    // Flags (always 0 for right now)
    address += MemoryLayout<Int32>.size

    let attributeAddr = address.relativeIndirectableAddress(
      as: ContextDescriptor.self
    )

    let attributeDescriptor = ContextDescriptor(attributeAddr)

    // Attribute Context Descriptor
    address += MemoryLayout<Int32>.size

    let numberOfInstances = address.loadUnaligned(as: UInt32.self)

    // Number of records this attribute has
    address += MemoryLayout<Int32>.size

    var fnPtrs: [UnsafeRawPointer] = []
    fnPtrs.reserveCapacity(Int(numberOfInstances))

    for _ in 0 ..< numberOfInstances {
      // The type this attribute was on (not always an actual type)
      address += MemoryLayout<Int32>.size

      var fnRecord = address.relativeDirectAddress(as: UnsafeRawPointer.self)
      fnRecord += MemoryLayout<RelativeDirectPointer<Void>>.size * 3

      let fnPtr = fnRecord.relativeDirectAddress(as: UnsafeRawPointer.self)
      fnPtrs.append(fnPtr)

      // Function pointer to attribute initializer
      address += MemoryLayout<Int32>.size
    }

    let copyFnPtrs = fnPtrs

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
    attributeCache.withLock {
      $0.map[attributeDescriptor, default: []].append(contentsOf: copyFnPtrs)
    }
#else
    // For non-Darwin platforms, we have to iterate the images and register
    // the attributes inside of the attribute cache. So, we're already in the
    // lock by the time this function gets called, so it is safe to access the
    // value in our lock without locking.
    attributeCache.withUnsafeValue {
      $0.map[attributeDescriptor, default: []].append(contentsOf: copyFnPtrs)
    }
#endif
  }
}
