//===--- Base64.swift -----------------------------------------*- swift -*-===//
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
//  Encode and decode sequences of bytes as base64.
//
//===----------------------------------------------------------------------===//

import Swift

// Forward mapping (we encode normal base64)
fileprivate let forwardMapping: (
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit,
  UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit
) = (
  // A     B     C     D     E     F     G     H
  0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
  // I     J     K     L     M     N     O     P
  0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50,
  // Q     R     S     T     U     V     W     X
  0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
  // Y     Z     a     b     c     d     e     f
  0x59, 0x5a, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66,
  // g     h     i     j     k     l     m     n
  0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e,
  // o     p     q     r     s     t     u     v
  0x6f, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76,
  // w     x     y     z     0     1     2     3
  0x77, 0x78, 0x79, 0x7a, 0x30, 0x31, 0x32, 0x33,
  // 4     5     6     7     8     9     +     /
  0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x2b, 0x2f
)

fileprivate func forward(at ndx: Int) -> UTF8.CodeUnit {
  precondition(ndx >= 0 && ndx < 64)
  return withUnsafePointer(to: forwardMapping) {
    $0.withMemoryRebound(to: UTF8.CodeUnit.self,
                         capacity: 64) { table in
      return table[ndx]
    }
  }
}

// Reverse (we support URL-safe and normal base64)
fileprivate let reverseMapping: (
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8,
  Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8
) = (
//
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
//                                               +       -       /
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, 62, -1, 63,
//   0   1   2   3   4   5   6   7   8   9
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
//       A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
//   P   Q   R   S   T   U   V   W   X   Y   Z                   _
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, 63,
//       a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
//   p   q   r   s   t   u   v   w   x   y   z
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1
)

fileprivate func reverse(at char: UTF8.CodeUnit) -> UInt8? {
  if char >= 128 {
    return nil
  }
  return withUnsafePointer(to: reverseMapping) {
    $0.withMemoryRebound(to: Int8.self,
                         capacity: 128) {
      (table: UnsafePointer<Int8>) -> UInt8? in

      let value = table[Int(char)]
      guard value >= 0 else {
        return nil
      }
      return UInt8(truncatingIfNeeded: value)
    }
  }
}

@_spi(Base64)
public struct Base64Encoder<S: Sequence>: Sequence
  where S.Element == UInt8
{
  public typealias Element = UTF8.CodeUnit

  var source: S

  public init(source: S) {
    self.source = source
  }

  public func makeIterator() -> Iterator {
    return Iterator(source: source)
  }

  public struct Iterator: IteratorProtocol {
    public typealias Element = UTF8.CodeUnit

    var sourceIterator: S.Iterator
    var output: (UTF8.CodeUnit, UTF8.CodeUnit, UTF8.CodeUnit)
    var ndx: Int
    var buffer: UInt32
    var count: Int

    init(source: S) {
      sourceIterator = source.makeIterator()
      output = (0, 0, 0)
      buffer = 0
      count = 0
      ndx = 3
    }

    public mutating func next() -> UTF8.CodeUnit? {
      // If we have bytes, output those first
      switch ndx {
        case 0:
          ndx += 1
          return output.0
        case 1:
          ndx += 1
          return output.1
        case 2:
          ndx += 1
          return output.2
        default:
          break
      }

      // Now try to refill the buffer with up to three bytes
      buffer = 0
      count = 0
      while count < 3 {
        if let byte = sourceIterator.next() {
          buffer = (buffer << 8) | UInt32(truncatingIfNeeded: byte)
        } else {
          break
        }
        count += 1
      }

      switch count {
        case 0:
          return nil
        case 1:
          let first = Int(buffer >> 2)
          let second = Int((buffer << 4) & 0x3f)
          output.2 = forward(at: second)
          ndx = 2
          return forward(at: first)
        case 2:
          let first = Int(buffer >> 10)
          let second = Int((buffer >> 4) & 0x3f)
          let third = Int((buffer << 2) & 0x3f)
          output.1 = forward(at: second)
          output.2 = forward(at: third)
          ndx = 1
          return forward(at: first)
        case 3:
          let first = Int(buffer >> 18)
          let second = Int((buffer >> 12) & 0x3f)
          let third = Int((buffer >> 6) & 0x3f)
          let fourth = Int(buffer & 0x3f)
          output.0 = forward(at: second)
          output.1 = forward(at: third)
          output.2 = forward(at: fourth)
          ndx = 0
          return forward(at: first)
        default:
          fatalError("count has an impossible value")
      }
    }
  }
}

@_spi(Base64)
public struct Base64Decoder<S: Sequence>: Sequence
  where S.Element == UTF8.CodeUnit
{
  public typealias Element = UInt8

  var source: S

  public init(source: S) {
    self.source = source
  }

  public func makeIterator() -> Iterator {
    return Iterator(source: source)
  }

  public struct Iterator: IteratorProtocol {
    public typealias Element = UInt8

    var sourceIterator: S.Iterator
    var output: (UInt8, UInt8)
    var ndx: Int
    var buffer: UInt32
    var count: Int
    var bad: Bool
    var done: Bool

    init(source: S) {
      sourceIterator = source.makeIterator()
      output = (0, 0)
      ndx = 2
      buffer = 0
      count = 0
      bad = false
      done = false
    }

    public mutating func next() -> UInt8? {
      if bad {
        return nil
      }

      // If we have bytes, output those first
      switch ndx {
        case 0:
          ndx += 1
          return output.0
        case 1:
          ndx += 1
          return output.1
        default:
          break
      }

      // If we've finished, stop
      if done {
        return nil
      }

      // Now try to refill the buffer
      count = 0
      while count < 4 {
        if let encoded = sourceIterator.next() {
          if encoded >= 128 {
            bad = true
            return nil
          }

          // '='
          if encoded == 0x3d {
            break
          }

          guard let value = reverse(at: encoded) else {
            bad = true
            return nil
          }

          buffer = (buffer << 6) | UInt32(truncatingIfNeeded: value)
          count += 1
        } else {
          break
        }
      }

      switch count {
        case 0:
          return nil
        case 1:
          bad = true
          return nil
        case 2:
          // 12 bits
          done = true
          return UInt8(truncatingIfNeeded: buffer >> 4)
        case 3:
          // 18 bits
          done = true
          let first = UInt8(truncatingIfNeeded: buffer >> 10)
          let second = UInt8(truncatingIfNeeded: buffer >> 2)
          output.1 = second
          ndx = 1
          return first
        case 4:
          // 24 bits
          let first = UInt8(truncatingIfNeeded: buffer >> 16)
          let second = UInt8(truncatingIfNeeded: buffer >> 8)
          let third = UInt8(truncatingIfNeeded: buffer)
          output.0 = second
          output.1 = third
          ndx = 0
          return first
        default:
          fatalError("count has an impossible value")
      }
    }
  }
}
