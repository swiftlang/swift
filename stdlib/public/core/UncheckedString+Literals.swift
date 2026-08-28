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
  /// data, matching `init(immortalString:)`'s requirements.
  @_specialize(where Element == UInt8)
  @_specialize(where Element == CChar)
  @_specialize(where Element == UInt16)
  @_effects(readonly)
  public init(
    _builtinUncheckedStringLiteral start: Builtin.RawPointer,
    unitCount: Builtin.Word
  ) {
    let buffer = unsafe UnsafeBufferPointer<Element>(
      start: UnsafeRawPointer(start).assumingMemoryBound(to: Element.self),
      count: Int(unitCount)
    )
    unsafe self.init(immortalString: buffer)
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
