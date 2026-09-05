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
extension UncheckedString: _ExpressibleByBuiltinUncheckedStringLiteral {
  /// Creates a string from the code units of a literal, which the compiler
  /// has already transcoded (for `\u{hh}` escapes and plain text) and
  /// spliced (for `\x{hh}` raw code unit escapes) to `Element`'s width.
  ///
  /// The pointee is permanently alive, backed by the executable's constant
  /// data, matching `init(immortalString:)`'s requirements. IRGen also
  /// guarantees a `0`-valued `Element` immediately follows the literal's
  /// data, so the result is marked NUL-terminated.
  ///
  /// - Parameters:
  ///   - start: A pointer to the literal's code units.
  ///   - unitCount: The number of code units at `start`.
  @_specialize(where Element == UInt8)
  @_specialize(where Element == CChar)
  @_specialize(where Element == UInt16)
  @_effects(readonly)
  @inlinable
  public init(
    _builtinUncheckedStringLiteral start: Builtin.RawPointer,
    unitCount: Builtin.Word
  ) {
    let buffer = unsafe UnsafeBufferPointer<Element>(
      start: UnsafeRawPointer(start).assumingMemoryBound(to: Element.self),
      count: Int(unitCount)
    )
    unsafe self.init(immortalString: buffer, nulTerminated: true)
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString: ExpressibleByUncheckedStringLiteral {
  /// The type of an unchecked string literal.
  public typealias UncheckedStringLiteralType = UncheckedString<Element>

  /// Creates an instance initialized to the given unchecked string value.
  ///
  /// - Parameter value: The value of the new instance.
  @_transparent
  public init(uncheckedStringLiteral value: UncheckedString<Element>) {
    self = value
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// The type used to build an `UncheckedString` from a string literal
  /// containing interpolations.
  ///
  /// Literal segments materialize as `UncheckedString<Element>` values
  /// directly (rather than `String`), so a `\x{hh}` raw code unit escape in
  /// a literal segment is fully supported, exactly as in a non-interpolated
  /// `UncheckedString` literal. Interpolated values must conform to
  /// `CustomUncheckedStringConvertible` with a matching `Element` -- there
  /// is no generic/reflective fallback, since describing an arbitrary value
  /// as text would require an encoding `UncheckedString` deliberately
  /// doesn't have.
  public struct StringInterpolation: StringInterpolationProtocol {
    /// The character data accumulated from the literal's segments so far.
    @usableFromInline
    internal var chars: [Element]

    /// Creates an interpolation with storage reserved for the given
    /// expected literal and interpolation sizes.
    ///
    /// - Parameters:
    ///   - literalCapacity: The approximate size of all literal segments
    ///                      combined.
    ///   - interpolationCount: The number of interpolations expected.
    public init(literalCapacity: Int, interpolationCount: Int) {
      chars = []
      chars.reserveCapacity(literalCapacity + interpolationCount)
    }

    /// Appends a literal segment of the string.
    ///
    /// - Parameter literal: A string literal containing the characters
    ///                       that appear next in the string literal.
    public mutating func appendLiteral(_ literal: UncheckedString<Element>) {
      literal.withCharacterData { data in
        data.withUnsafeBufferPointer { buffer in
          unsafe chars.append(contentsOf: buffer)
        }
      }
    }

    /// Appends the raw representation of `value`, which must produce
    /// `Element`s directly -- no encoding, transcoding, or textual
    /// description is involved.
    ///
    /// - Parameter value: The value to append.
    public mutating func appendInterpolation<T: CustomUncheckedStringConvertible>(
      _ value: T
    ) where T.UncheckedStringElement == Element {
      value.withUncheckedStringRepresentation { data in
        data.withUnsafeBufferPointer { buffer in
          unsafe chars.append(contentsOf: buffer)
        }
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString: ExpressibleByUncheckedStringInterpolation {
  /// Creates an instance from a string interpolation.
  ///
  /// - Parameter stringInterpolation: An instance of `StringInterpolation`
  ///             which has had each segment of the string literal appended
  ///             to it.
  public init(stringInterpolation: StringInterpolation) {
    self.init(taking: stringInterpolation.chars)
  }
}
