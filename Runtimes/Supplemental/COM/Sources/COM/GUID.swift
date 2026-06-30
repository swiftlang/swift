//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A 128-bit Globally Unique Identifier (GUID) with compile-time type safety
/// through generic tagging.
///
/// `GUID` is a generic structure that holds the four components of a standard
/// UUID, organized according to RFC 4122. The `Tag` generic parameter is a
/// phantom type used to distinguish different kinds of GUIDs (such as Interface
/// Identifiers and Class Identifiers) at compile time without any runtime
/// overhead.
///
/// This is an internal type meant to be used through type aliases like `IID`
/// and `CLSID`. Direct use outside of the module is not recommended.
///
/// ## Structure
///
/// A GUID consists of:
/// - `data1`: A 32-bit value
/// - `data2`: A 16-bit value
/// - `data3`: A 16-bit value
/// - `data4`: An 8-byte array
///
/// These four components form the standard 128-bit UUID representation.
@frozen
public struct GUID<Tag> {
  /// The first 32-bit component of the GUID.
  public let data1: UInt32
  /// The first 16-bit component of the GUID.
  public let data2: UInt16
  /// The second 16-bit component of the GUID.
  public let data3: UInt16
  /// The final 8-byte array component of the GUID.
  public let data4: InlineArray<8, UInt8>

  /// Creates a GUID with the four standard UUID components.
  ///
  /// - Parameters:
  ///   - data1: The first 32-bit component
  ///   - data2: The first 16-bit component
  ///   - data3: The second 16-bit component
  ///   - data4: The final 8-byte array component
  @inlinable @_transparent
  internal init(data1: UInt32, data2: UInt16, data3: UInt16,
                data4: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)) {
    self.data1 = data1
    self.data2 = data2
    self.data3 = data3
    self.data4 = unsafeBitCast(data4, to: InlineArray<8, UInt8>.self)
  }
}

extension GUID: Equatable {
  @inlinable
  public static func == (_ lhs: borrowing GUID, _ rhs: borrowing GUID) -> Bool {
    unsafeBitCast(lhs, to: UInt128.self) == unsafeBitCast(rhs, to: UInt128.self)
  }
}

extension GUID: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(unsafeBitCast(self, to: UInt128.self))
  }
}

extension GUID: Sendable {
}

extension GUID: CustomStringConvertible {
  @inlinable
  public var description: String {
    String(unsafeUninitializedCapacity: 36) { buffer in
      let characters: StaticString = "0123456789abcdef"

      @inline(__always)
      func emit(byte value: UInt8, at offset: UnsafeMutableBufferPointer<UInt8>.Index) {
        let hi = Int((value >> 4) & 0xf)
        let lo = Int((value >> 0) & 0xf)
        buffer[offset + 0] = characters.utf8Start.advanced(by: hi).pointee
        buffer[offset + 1] = characters.utf8Start.advanced(by: lo).pointee
      }

      emit(byte: UInt8(truncatingIfNeeded: data1 >> 24), at: 0)
      emit(byte: UInt8(truncatingIfNeeded: data1 >> 16), at: 2)
      emit(byte: UInt8(truncatingIfNeeded: data1 >>  8), at: 4)
      emit(byte: UInt8(truncatingIfNeeded: data1 >>  0), at: 6)
      buffer[8] = UInt8(ascii: "-")
      emit(byte: UInt8(truncatingIfNeeded: data2 >>  8), at: 9)
      emit(byte: UInt8(truncatingIfNeeded: data2 >>  0), at: 11)
      buffer[13] = UInt8(ascii: "-")
      emit(byte: UInt8(truncatingIfNeeded: data3 >>  8), at: 14)
      emit(byte: UInt8(truncatingIfNeeded: data3 >>  0), at: 16)
      buffer[18] = UInt8(ascii: "-")
      emit(byte: data4[0], at: 19)
      emit(byte: data4[1], at: 21)
      buffer[23] = UInt8(ascii: "-")
      emit(byte: data4[2], at: 24)
      emit(byte: data4[3], at: 26)
      emit(byte: data4[4], at: 28)
      emit(byte: data4[5], at: 30)
      emit(byte: data4[6], at: 32)
      emit(byte: data4[7], at: 34)
      return 36
    }
  }
}
