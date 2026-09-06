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

import SwiftShims

//
// `==`, `<`, and `hash(into:)` are defined only in the generic
// `UncheckedStringProtocol` extension (see UncheckedStringProtocol.swift),
// are not `@inlinable`, and have no concrete override on `UncheckedString`/
// `UncheckedSubString`. That means a call like `a == b`, even with `a`/`b`
// concretely typed at the call site, still pays witness-table dispatch for
// the inner `withCharacterData` call. This file adds a single concrete,
// `@inlinable` override of each directly on `UncheckedString`/
// `UncheckedSubString`, generic over any `Element: FixedWidthInteger`:
//
//  - `==`/`hash(into:)` get a genuine algorithmic speedup for *every*
//    width: a bulk raw-byte comparison/hash is correct for any fixed-width
//    element, since two values are equal iff their raw byte representations
//    are equal (both written by the same process/platform), and `Hashable`
//    only requires "equal values hash equal," which holds trivially for a
//    hash of the raw bytes.
//  - `<` cannot use a raw byte compare unconditionally -- for multi-byte
//    elements, byte-order comparison isn't equivalent to numeric
//    element-order comparison, and even for single-byte elements it's only
//    valid for an *unsigned* one (`Int8`/`CChar`'s byte ordering and signed
//    numeric ordering disagree on negative values) -- so it keeps a
//    per-element loop as the general case, but special-cases
//    `Element.self == UInt8.self` specifically (a compile-time-foldable
//    check once `Element` is concretely known at a specialized call site)
//    to reuse the same memcmp-based fast path as `==`.
//
// Each operator has a single implementation, with the `UInt8` case handled
// by a runtime branch inside it rather than a second, more specific
// extension: Swift's witness selection for `Equatable`/`Comparable`'s
// operator requirements does not reliably prefer a more-constrained sibling
// extension over a generic one on the same concrete type, so splitting
// these into two competing extensions doesn't reliably dispatch to the
// specialized one anyway.
//
// Getting actual *data* out of `.small` storage fast for single-byte
// elements (as opposed to just picking a fast comparison algorithm once the
// data is already in hand) is handled inside `withCharacterData` itself
// (UncheckedString.swift), not here.
//
// `UncheckedSubString` gets the identical trio below, for the identical
// reason (its own conformances also come only from `UncheckedStringProtocol`'s
// generic default otherwise). Its `withCharacterData` just forwards to
// `base.withCharacterData`, so it doesn't need its own duplicate of that
// method's fast path.

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Returns a Boolean value indicating whether two strings are equal.
  ///
  /// - Parameters:
  ///   - lhs: A string to compare.
  ///   - rhs: Another string to compare.
  ///
  /// - Returns `true` if `lhs` and `rhs` contain the same elements;
  ///           otherwise, `false`.
  @inlinable
  public static func == (lhs: Self, rhs: Self) -> Bool {
    if lhs.count != rhs.count { return false }
    return lhs.withCharacterData { l in
      rhs.withCharacterData { r in
        l.withUnsafeBufferPointer { lb in
          r.withUnsafeBufferPointer { rb in
            let lRaw = unsafe UnsafeRawBufferPointer(lb)
            let rRaw = unsafe UnsafeRawBufferPointer(rb)
            if unsafe lRaw.isEmpty { return true }
            return unsafe 0 == _swift_stdlib_memcmp(
              lRaw.baseAddress.unsafelyUnwrapped,
              rRaw.baseAddress.unsafelyUnwrapped,
              lRaw.count)
          }
        }
      }
    }
  }

  /// Returns a Boolean value indicating whether the first string is
  /// ordered before the second, by elementwise comparison.
  ///
  /// - Parameters:
  ///   - lhs: A string to compare.
  ///   - rhs: Another string to compare.
  ///
  /// - Returns `true` if `lhs` is ordered before `rhs`; otherwise, `false`.
  @inlinable
  public static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.withCharacterData { l in
      rhs.withCharacterData { r in
        l.withUnsafeBufferPointer { lb in
          r.withUnsafeBufferPointer { rb in
            if Element.self == UInt8.self {
              let lRaw = unsafe UnsafeRawBufferPointer(lb)
              let rRaw = unsafe UnsafeRawBufferPointer(rb)
              let n = Swift.min(lRaw.count, rRaw.count)
              let cmp = n == 0 ? 0 : unsafe _swift_stdlib_memcmp(
                lRaw.baseAddress.unsafelyUnwrapped,
                rRaw.baseAddress.unsafelyUnwrapped,
                n)
              return cmp != 0 ? cmp < 0 : lb.count < rb.count
            }
            let minCount = Swift.min(lb.count, rb.count)
            var i = 0
            while i < minCount {
              let lv = unsafe lb[i]
              let rv = unsafe rb[i]
              if lv != rv { return lv < rv }
              i += 1
            }
            return lb.count < rb.count
          }
        }
      }
    }
  }

  /// Hashes the essential components of this string by feeding them into
  /// the given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///                      of this string.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(count)
    withCharacterData { data in
      data.withUnsafeBufferPointer { buffer in
        unsafe hasher.combine(bytes: UnsafeRawBufferPointer(buffer))
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedSubString {
  /// Returns a Boolean value indicating whether two substrings are equal.
  ///
  /// - Parameters:
  ///   - lhs: A substring to compare.
  ///   - rhs: Another substring to compare.
  ///
  /// - Returns `true` if `lhs` and `rhs` contain the same elements;
  ///           otherwise, `false`.
  @inlinable
  public static func == (lhs: Self, rhs: Self) -> Bool {
    if lhs.count != rhs.count { return false }
    return lhs.withCharacterData { l in
      rhs.withCharacterData { r in
        l.withUnsafeBufferPointer { lb in
          r.withUnsafeBufferPointer { rb in
            let lRaw = unsafe UnsafeRawBufferPointer(lb)
            let rRaw = unsafe UnsafeRawBufferPointer(rb)
            if unsafe lRaw.isEmpty { return true }
            return unsafe 0 == _swift_stdlib_memcmp(
              lRaw.baseAddress.unsafelyUnwrapped,
              rRaw.baseAddress.unsafelyUnwrapped,
              lRaw.count)
          }
        }
      }
    }
  }

  /// Returns a Boolean value indicating whether the first substring is
  /// ordered before the second, by elementwise comparison.
  ///
  /// - Parameters:
  ///   - lhs: A substring to compare.
  ///   - rhs: Another substring to compare.
  ///
  /// - Returns `true` if `lhs` is ordered before `rhs`; otherwise, `false`.
  @inlinable
  public static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.withCharacterData { l in
      rhs.withCharacterData { r in
        l.withUnsafeBufferPointer { lb in
          r.withUnsafeBufferPointer { rb in
            if Element.self == UInt8.self {
              let lRaw = unsafe UnsafeRawBufferPointer(lb)
              let rRaw = unsafe UnsafeRawBufferPointer(rb)
              let n = Swift.min(lRaw.count, rRaw.count)
              let cmp = n == 0 ? 0 : unsafe _swift_stdlib_memcmp(
                lRaw.baseAddress.unsafelyUnwrapped,
                rRaw.baseAddress.unsafelyUnwrapped,
                n)
              return cmp != 0 ? cmp < 0 : lb.count < rb.count
            }
            let minCount = Swift.min(lb.count, rb.count)
            var i = 0
            while i < minCount {
              let lv = unsafe lb[i]
              let rv = unsafe rb[i]
              if lv != rv { return lv < rv }
              i += 1
            }
            return lb.count < rb.count
          }
        }
      }
    }
  }

  /// Hashes the essential components of this substring by feeding them
  /// into the given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///                      of this substring.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(count)
    withCharacterData { data in
      data.withUnsafeBufferPointer { buffer in
        unsafe hasher.combine(bytes: UnsafeRawBufferPointer(buffer))
      }
    }
  }
}
