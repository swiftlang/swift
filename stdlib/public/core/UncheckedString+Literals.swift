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

/// The default type used to build an `UncheckedString` from a string
/// literal containing interpolations.
///
/// When implementing an `ExpressibleByUncheckedStringInterpolation`
/// conformance whose `UncheckedStringLiteralType` is
/// `UncheckedString<Element>`, set the `StringInterpolation` associated
/// type to `DefaultUncheckedStringInterpolation<Element>` to get the same
/// interpolation behavior `UncheckedString` itself uses.
///
/// Literal segments materialize as `UncheckedString<Element>` values
/// directly (rather than `String`), so a `\x{hh}` raw code unit escape in
/// a literal segment is fully supported, exactly as in a non-interpolated
/// `UncheckedString` literal. Interpolated values must conform to
/// `CustomUncheckedStringConvertible` with a matching `Element` -- there
/// is no generic/reflective fallback, since describing an arbitrary value
/// as text would require an encoding `UncheckedString` deliberately
/// doesn't have.
@available(SwiftStdlib 9999, *)
@frozen
public struct DefaultUncheckedStringInterpolation<Element: FixedWidthInteger>
  : StringInterpolationProtocol {
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
  @inlinable
  public init(literalCapacity: Int, interpolationCount: Int) {
    chars = []
    chars.reserveCapacity(literalCapacity + interpolationCount)
  }

  /// Appends a literal segment of the string.
  ///
  /// - Parameter literal: A string literal containing the characters
  ///                       that appear next in the string literal.
  @inlinable
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
  @inlinable
  public mutating func appendInterpolation<T: CustomUncheckedStringConvertible>(
    _ value: T
  ) where T.UncheckedStringElement == Element {
    value.withUncheckedStringRepresentation { data in
      data.withUnsafeBufferPointer { buffer in
        unsafe chars.append(contentsOf: buffer)
      }
    }
  }

  /// Creates an `UncheckedString` from this instance, consuming the
  /// instance in the process.
  @inlinable
  internal __consuming func make() -> UncheckedString<Element> {
    return UncheckedString(taking: chars)
  }
}

@available(SwiftStdlib 9999, *)
extension DefaultUncheckedStringInterpolation: Sendable where Element: Sendable {}

/// Exposes `DefaultUncheckedStringInterpolation`'s `Element` as an
/// associated type, so `ExpressibleByUncheckedStringInterpolation`'s default
/// `init(stringInterpolation:)` below can refer to it via ordinary
/// associated-type projection (`StringInterpolation._UncheckedElement`)
/// instead of introducing a fresh generic parameter of its own.
@available(SwiftStdlib 9999, *)
public protocol _DefaultUncheckedStringInterpolationProtocol {
  associatedtype _UncheckedElement: FixedWidthInteger

  __consuming func _makeUncheckedString() -> UncheckedString<_UncheckedElement>
}

@available(SwiftStdlib 9999, *)
extension DefaultUncheckedStringInterpolation: _DefaultUncheckedStringInterpolationProtocol {
  public typealias _UncheckedElement = Element

  @inlinable
  public __consuming func _makeUncheckedString() -> UncheckedString<Element> {
    return make()
  }
}

@available(SwiftStdlib 9999, *)
extension ExpressibleByUncheckedStringInterpolation
  where StringInterpolation: _DefaultUncheckedStringInterpolationProtocol,
    UncheckedStringLiteralType
      == UncheckedString<StringInterpolation._UncheckedElement> {
  /// Creates a new instance from an interpolated unchecked string literal.
  ///
  /// Don't call this initializer directly. It's used by the compiler when
  /// you create an unchecked string literal containing interpolations, for
  /// conforming types that reuse `DefaultUncheckedStringInterpolation` --
  /// i.e. that set their `StringInterpolation` associated type to
  /// `DefaultUncheckedStringInterpolation<Element>` and their
  /// `UncheckedStringLiteralType` to `UncheckedString<Element>`, for the
  /// same `Element`, to get the same interpolation behavior
  /// `UncheckedString` itself uses.
  ///
  /// - Parameter stringInterpolation: An instance of
  ///             `DefaultUncheckedStringInterpolation` which has had each
  ///             segment of the string literal appended to it.
  public init(stringInterpolation: StringInterpolation) {
    self.init(uncheckedStringLiteral: stringInterpolation._makeUncheckedString())
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// The type used to build an `UncheckedString` from a string literal
  /// containing interpolations.
  public typealias StringInterpolation = DefaultUncheckedStringInterpolation<Element>
}

@available(SwiftStdlib 9999, *)
extension UncheckedString: ExpressibleByUncheckedStringInterpolation {
  /// Creates an instance from a string interpolation.
  ///
  /// - Parameter stringInterpolation: An instance of `StringInterpolation`
  ///             which has had each segment of the string literal appended
  ///             to it.
  public init(stringInterpolation: StringInterpolation) {
    self = stringInterpolation.make()
  }
}
