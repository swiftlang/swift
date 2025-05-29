//===--- Address.swift ----------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines the `Backtrace.Address` struct that represents addresses in a
//  captured backtrace.  This type is *not* used for storage; rather, it's
//  used as an interface type.
//
//===----------------------------------------------------------------------===//

import Swift

// .. Comparable .............................................................

extension Backtrace.Address {
  fileprivate var widestRepresentation: UInt64 {
    switch representation {
      case .null:
        return 0
      case let .sixteenBit(addr):
        return UInt64(addr)
      case let .thirtyTwoBit(addr):
        return UInt64(addr)
      case let .sixtyFourBit(addr):
        return addr
    }
  }
}

extension Backtrace.Address: Comparable {
  /// Return true if `lhs` is lower than `rhs`
  public static func < (lhs: Backtrace.Address, rhs: Backtrace.Address) -> Bool {
    return lhs.widestRepresentation < rhs.widestRepresentation
  }
  /// Return true if `lhs` is equal to `rhs`
  public static func == (lhs: Backtrace.Address, rhs: Backtrace.Address) -> Bool {
    return lhs.widestRepresentation == rhs.widestRepresentation
  }
}

// .. LosslessStringConvertible ..............................................

extension Backtrace.Address: LosslessStringConvertible {
  /// Create an Backtrace.Address from its string representation
  public init?(_ s: String) {
    self.init(s[...])
  }

  public init?(_ s: Substring) {
    let unprefixed: Substring

    // Explicit support for null
    if s == "null" {
      self.representation = .null
      return
    }

    // Drop the prefix, if any
    if s.hasPrefix("0x") {
      unprefixed = s[s.index(s.startIndex, offsetBy: 2)...]
    } else {
      unprefixed = Substring(s)
    }

    // Work out whether it's 64-bit or 32-bit and parse it
    if unprefixed.count > 8 && unprefixed.count <= 16 {
      guard let addr = UInt64(unprefixed, radix: 16) else {
        return nil
      }
      if addr == 0 {
        self.representation = .null
      } else {
        self.representation = .sixtyFourBit(addr)
      }
    } else if unprefixed.count <= 8 {
      guard let addr = UInt32(unprefixed, radix: 16) else {
        return nil
      }
      if addr == 0 {
        self.representation = .null
      } else {
        self.representation = .thirtyTwoBit(addr)
      }
    } else {
      return nil
    }
  }

  /// A textual representation of this address
  public var description: String {
    switch representation {
      case .null:
        return "null"
      case let .sixteenBit(addr):
        if addr == 0 {
          return "null"
        }
        return hex(addr)
      case let .thirtyTwoBit(addr):
        if addr == 0 {
          return "null"
        }
        return hex(addr)
      case let .sixtyFourBit(addr):
        if addr == 0 {
          return "null"
        }
        return hex(addr)
    }
  }
}

// .. ExpressibleByIntegerLiteral ............................................

extension Backtrace.Address: ExpressibleByIntegerLiteral {
  public typealias IntegerLiteralType = UInt64

  /// Convert from an integer literal.
  public init(integerLiteral: Self.IntegerLiteralType) {
    if integerLiteral == 0 {
      self.representation = .null
    } else if integerLiteral < 0x10000 {
      self.representation = .sixteenBit(UInt16(truncatingIfNeeded: integerLiteral))
    } else if integerLiteral < 0x100000000 {
      self.representation = .thirtyTwoBit(UInt32(truncatingIfNeeded: integerLiteral))
    } else {
      self.representation = .sixtyFourBit(integerLiteral)
    }
  }
}

// .. FixedWidthInteger conversions ..........................................

extension Backtrace.Address {
  fileprivate func toFixedWidth<T: FixedWidthInteger>(
    type: T.Type = T.self
  ) -> T? {
    switch representation {
      case .null:
        return T(0)
      case let .sixteenBit(addr):
        guard T.bitWidth >= 16 else { return nil }
        return T(truncatingIfNeeded: addr)
      case let .thirtyTwoBit(addr):
        guard T.bitWidth >= 32 else { return nil }
        return T(truncatingIfNeeded: addr)
      case let .sixtyFourBit(addr):
        guard T.bitWidth >= 64 else { return nil }
        return T(truncatingIfNeeded: addr)
    }
  }
}

extension FixedWidthInteger {
  /// Convert from an Backtrace.Address.
  ///
  /// This initializer will return nil if the address width is larger than the
  /// type you are attempting to convert into.
  public init?(_ address: Backtrace.Address) {
    guard let result = address.toFixedWidth(type: Self.self) else {
      return nil
    }
    self = result
  }
}

extension Backtrace.Address {
  /// Convert from a UInt16.
  public init(_ value: UInt16) {
    if value == 0 {
      self.representation = .null
      return
    }
    self.representation = .sixteenBit(value)
  }

  /// Convert from a UInt32.
  public init(_ value: UInt32) {
    if value == 0 {
      self.representation = .null
      return
    }
    self.representation = .thirtyTwoBit(value)
  }

  /// Convert from a UInt64.
  public init(_ value: UInt64) {
    if value == 0 {
      self.representation = .null
      return
    }
    self.representation = .sixtyFourBit(value)
  }

  /// Convert from a FixedWidthInteger
  public init?<T: FixedWidthInteger>(_ value: T) {
    switch T.bitWidth {
      case 16:
        self.init(UInt16(truncatingIfNeeded: value))
      case 32:
        self.init(UInt32(truncatingIfNeeded: value))
      case 64:
        self.init(UInt64(truncatingIfNeeded: value))
      default:
        return nil
    }
  }
}

// -- Arithmetic -------------------------------------------------------------

extension Backtrace.Address {
  static func - (lhs: Backtrace.Address, rhs: Backtrace.Address) -> Int64 {
    let ulhs = UInt64(lhs)!
    let urhs = UInt64(rhs)!
    return Int64(bitPattern: ulhs - urhs)
  }

  static func - (lhs: Backtrace.Address, rhs: Int64) -> Backtrace.Address {
    switch lhs.representation {
      case .null:
        return Backtrace.Address(0)
      case let .sixteenBit(addr):
        let newAddr = addr &- UInt16(bitPattern: Int16(truncatingIfNeeded: rhs))
        return Backtrace.Address(newAddr)
      case let .thirtyTwoBit(addr):
        let newAddr = addr &- UInt32(bitPattern: Int32(truncatingIfNeeded: rhs))
        return Backtrace.Address(newAddr)
      case let .sixtyFourBit(addr):
        let newAddr = addr &- UInt64(bitPattern: rhs)
        return Backtrace.Address(newAddr)
    }
  }

  static func + (lhs: Backtrace.Address, rhs: Int64) -> Backtrace.Address {
    switch lhs.representation {
      case .null:
        return Backtrace.Address(0)
      case let .sixteenBit(addr):
        let newAddr = addr &+ UInt16(bitPattern: Int16(truncatingIfNeeded: rhs))
        return Backtrace.Address(newAddr)
      case let .thirtyTwoBit(addr):
        let newAddr = addr &+ UInt32(bitPattern: Int32(truncatingIfNeeded: rhs))
        return Backtrace.Address(newAddr)
      case let .sixtyFourBit(addr):
        let newAddr = addr &+ UInt64(bitPattern: rhs)
        return Backtrace.Address(newAddr)
    }
  }
}
