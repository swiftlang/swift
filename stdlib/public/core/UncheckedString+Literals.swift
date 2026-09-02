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
  public typealias UncheckedStringLiteralType = UncheckedString<Element>

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
    @usableFromInline
    internal var chars: [Element]

    public init(literalCapacity: Int, interpolationCount: Int) {
      chars = []
      chars.reserveCapacity(literalCapacity + interpolationCount)
    }

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
  public init(stringInterpolation: StringInterpolation) {
    self.init(taking: stringInterpolation.chars)
  }
}
