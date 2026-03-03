//===--- ByteOrder.swift --------------------------------------------------===//
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

/// A byte ordering in memory.
@frozen
public enum ByteOrder: Equatable, Hashable, Sendable {
  /// Bytes are ordered with the most significant bits
  /// starting at the lowest memory address
  case bigEndian

  /// Bytes are ordered with the least significant bits
  /// starting at the lowest memory address
  case littleEndian

  /// The native byte ordering for the runtime target.
  static var native: Self {
#if _endian(big)
    .bigEndian
#elseif _endian(little)
    .littleEndian
#else
    #error("endianness is undefined")
#endif
  }
}
