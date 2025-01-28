//===--- ImageMap.swift ----------------------------------------*- swift -*-===//
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
//  Defines the `ImageMap` struct that represents a captured list of loaded
//  images.
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
internal import Darwin
internal import BacktracingImpl.OS.Darwin
#endif

/// Holds a map of the process's address space.
public struct ImageMap: Collection, Sendable, Hashable {

  /// A type representing the sequence's elements.
  public typealias Element = Backtrace.Image

  /// A type that represents a position in the collection.
  public typealias Index = Int

  /// Tells us what size of machine words were used when capturing the
  /// image map.
  enum WordSize: Sendable {
    case sixteenBit
    case thirtyTwoBit
    case sixtyFourBit
  }

  /// We use UInt64s for addresses here.
  typealias Address = UInt64

  /// The internal representation of an image.
  struct Image: Sendable, Hashable {
    var name: String?
    var path: String?
    var uniqueID: [UInt8]?
    var baseAddress: Address
    var endOfText: Address
  }

  /// The name of the platform that captured this image map.
  public private(set) var platform: String

  /// The actual image storage.
  var images: [Image]

  /// The size of words used when capturing.
  var wordSize: WordSize

  /// Construct an ImageMap.
  init(platform: String, images: [Image], wordSize: WordSize) {
    self.platform = platform
    self.images = images
    self.wordSize = wordSize
  }

  /// Construct an ImageMap from CompactImageMap data {
  @_spi(Internal)
  public init?(compactImageMapData: some Sequence<UInt8>) {
    var decoder = CompactImageMapFormat.Decoder(compactImageMapData)
    guard let (platform, images, wordSize) = decoder.decode() else {
      return nil
    }
    self.init(platform: platform, images: images, wordSize: wordSize)
  }

  /// The position of the first element in a non-empty collection.
  public var startIndex: Self.Index {
    return 0
  }

  /// The collection's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  public var endIndex: Self.Index {
    return images.count
  }

  /// Accesses the element at the specified position.
  public subscript(_ ndx: Self.Index) -> Self.Element {
    return Backtrace.Image(images[ndx], wordSize: wordSize)
  }

  /// Look-up an image by address.
  public func indexOfImage(at address: Backtrace.Address) -> Int? {
    let addr = UInt64(address)!
    var lo = 0, hi = images.count
    while lo < hi {
      let mid = (lo + hi) / 2
      if images[mid].baseAddress > addr {
        hi = mid
      } else if images[mid].endOfText <= addr {
        lo = mid + 1
      } else {
        return mid
      }
    }

    return nil
  }

  /// Returns the position immediately after the given index.
  public func index(after ndx: Self.Index) -> Self.Index {
    return ndx + 1
  }

  /// Capture the image map for the current process.
  public static func capture() -> ImageMap {
    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    return capture(for: mach_task_self())
    #else
    return capture(using: UnsafeLocalMemoryReader())
    #endif
  }
}

extension ImageMap: CustomStringConvertible {
  /// Generate a description of an ImageMap
  public var description: String {
    var lines: [String] = ["Platform: \(platform)", ""]
    let addressWidth: Int
    switch wordSize {
      case .sixteenBit: addressWidth = 4
      case .thirtyTwoBit: addressWidth = 8
      case .sixtyFourBit: addressWidth = 16
    }

    for image in images {
      let hexBase = hex(image.baseAddress, width: addressWidth)
      let hexEnd = hex(image.endOfText, width: addressWidth)
      let buildId: String
      if let bytes = image.uniqueID {
        buildId =  hex(bytes)
      } else {
        buildId = "<no build ID>"
      }
      let path = image.path ?? "<unknown>"
      let name = image.name ?? "<unknown>"

      lines.append("\(hexBase)-\(hexEnd) \(buildId) \(name) \(path)")
    }

    return lines.joined(separator: "\n")
  }
}

extension Backtrace.Image {
  /// Convert an ImageMap.Image to a Backtrace.Image.
  ///
  /// Backtrace.Image is the public, user-visible type; ImageMap.Image
  /// is an in-memory representation.
  init(_ image: ImageMap.Image, wordSize: ImageMap.WordSize) {
    let baseAddress: Backtrace.Address
    let endOfText: Backtrace.Address

    switch wordSize {
      case .sixteenBit:
        baseAddress = Backtrace.Address(
          UInt16(truncatingIfNeeded: image.baseAddress)
        )
        endOfText = Backtrace.Address(
          UInt16(truncatingIfNeeded: image.endOfText)
        )
      case .thirtyTwoBit:
        baseAddress = Backtrace.Address(
          UInt32(truncatingIfNeeded: image.baseAddress)
        )
        endOfText = Backtrace.Address(
          UInt32(truncatingIfNeeded: image.endOfText)
        )
      case .sixtyFourBit:
        baseAddress = Backtrace.Address(image.baseAddress)
        endOfText = Backtrace.Address(image.endOfText)
    }

    self.init(name: image.name,
              path: image.path,
              uniqueID: image.uniqueID,
              baseAddress: baseAddress,
              endOfText: endOfText)
  }
}

extension ImageMap: Codable {

  public func encode(to encoder: any Encoder) throws {
    var container = encoder.singleValueContainer()
    let cimfEncoder = CompactImageMapFormat.Encoder(self)
    let base64 = stringFrom(sequence: Base64Encoder(source: cimfEncoder))
    try container.encode(base64)
  }

  public init(from decoder: any Decoder) throws {
    let container = try decoder.singleValueContainer()
    let base64 = try container.decode(String.self)
    self.init(compactImageMapData: Base64Decoder(source: base64.utf8))!
  }

}
