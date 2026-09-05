//===----------------------------------------------------------------------===//
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

@available(SwiftStdlib 9999, *)
extension UncheckedString: Encodable where Element: Encodable {
  /// Encodes this string's elements into the given encoder, as an unkeyed
  /// container of `Element`s.
  ///
  /// - Parameter encoder: The encoder to write data to.
  @inlinable
  public func encode(to encoder: any Encoder) throws {
    var container = encoder.unkeyedContainer()
    try withCharacterData { data in
      var i = 0
      while i < data.count {
        try container.encode(data[i])
        i += 1
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString: Decodable where Element: Decodable {
  /// Creates a new string by decoding from the given decoder.
  ///
  /// This initializer throws an error if reading from the decoder fails, or
  /// if the data read is corrupted or otherwise invalid.
  ///
  /// - Parameter decoder: The decoder to read data from.
  @inlinable
  public init(from decoder: any Decoder) throws {
    var container = try decoder.unkeyedContainer()
    var elements = [Element]()

    // Pre-size when the container can tell us how many elements are
    // coming, mirroring `Data.init(from:)`. We don't loop only up to
    // `count` the way `Data` does, since -- unlike `Data`'s fixed-size
    // preallocated buffer, which must be filled by index -- `elements` is
    // just an `Array` being built with `append`, so `reserveCapacity` is a
    // pure hint: an inaccurate `count` (in either direction) cannot cause
    // us to under- or over-fill anything.
    if let count = container.count {
      elements.reserveCapacity(count)
    }

    while !container.isAtEnd {
      elements.append(try container.decode(Element.self))
    }

    self.init(taking: elements)
  }
}
