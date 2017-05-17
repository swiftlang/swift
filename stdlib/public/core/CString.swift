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
// String interop with C
//===----------------------------------------------------------------------===//

import SwiftShims

extension StringProtocol {

  /// Creates a new string by copying the null-terminated UTF-8 data referenced
  /// by the given pointer.
  ///
  /// If `cString` contains ill-formed UTF-8 code unit sequences, this
  /// initializer replaces them with the Unicode replacement character
  /// (`"\u{FFFD}"`).
  ///
  /// The following example calls this initializer with pointers to the
  /// contents of two different `CChar` arrays---the first with well-formed
  /// UTF-8 code unit sequences and the second with an ill-formed sequence at
  /// the end.
  ///
  ///     let validUTF8: [CChar] = [67, 97, 102, -61, -87, 0]
  ///     validUTF8.withUnsafeBufferPointer { ptr in
  ///         let s = String(cString: ptr.baseAddress!)
  ///         print(s)
  ///     }
  ///     // Prints "Café"
  ///
  ///     let invalidUTF8: [CChar] = [67, 97, 102, -61, 0]
  ///     invalidUTF8.withUnsafeBufferPointer { ptr in
  ///         let s = String(cString: ptr.baseAddress!)
  ///         print(s)
  ///     }
  ///     // Prints "Caf�"
  ///
  /// - Parameter cString: A pointer to a null-terminated UTF-8 code sequence.
  public init(cString: UnsafePointer<CChar>) {
    let len = UTF8._nullCodeUnitOffset(in: cString)
    let (result, _) = cString.withMemoryRebound(to: UInt8.self, capacity: len) {
      _decodeCString(
        $0, as: UTF8.self, length: len, repairingInvalidCodeUnits: true)!
    }
    self = Self(String(result))
  }

  /// Creates a new string by copying the null-terminated UTF-8 data referenced
  /// by the given pointer.
  ///
  /// This is identical to init(cString: UnsafePointer<CChar> but operates on an
  /// unsigned sequence of bytes.
  public init(cString: UnsafePointer<UInt8>) {
    self = Self(
      String.decodeCString(
        cString, as: UTF8.self, repairingInvalidCodeUnits: true)!.result)
  }

  /// Creates a new string by copying and validating the null-terminated UTF-8
  /// data referenced by the given pointer.
  ///
  /// This initializer does not try to repair ill-formed UTF-8 code unit
  /// sequences. If any are found, the result of the initializer is `nil`.
  ///
  /// The following example calls this initializer with pointers to the
  /// contents of two different `CChar` arrays---the first with well-formed
  /// UTF-8 code unit sequences and the second with an ill-formed sequence at
  /// the end.
  ///
  ///     let validUTF8: [CChar] = [67, 97, 102, -61, -87, 0]
  ///     validUTF8.withUnsafeBufferPointer { ptr in
  ///         let s = String(validatingUTF8: ptr.baseAddress!)
  ///         print(s)
  ///     }
  ///     // Prints "Optional(Café)"
  ///
  ///     let invalidUTF8: [CChar] = [67, 97, 102, -61, 0]
  ///     invalidUTF8.withUnsafeBufferPointer { ptr in
  ///         let s = String(validatingUTF8: ptr.baseAddress!)
  ///         print(s)
  ///     }
  ///     // Prints "nil"
  ///
  /// - Parameter cString: A pointer to a null-terminated UTF-8 code sequence.
  public init?(validatingUTF8 cString: UnsafePointer<CChar>) {
    let len = UTF8._nullCodeUnitOffset(in: cString)
    guard let (result, _) =
    cString.withMemoryRebound(to: UInt8.self, capacity: len, {
        _decodeCString($0, as: UTF8.self, length: len,
          repairingInvalidCodeUnits: false)
      })
    else {
      return nil
    }
    self = Self(result)
  }

  /// Creates a new string by copying the null-terminated data referenced by
  /// the given pointer using the specified encoding.
  ///
  /// When you pass `true` as `isRepairing`, this method replaces ill-formed
  /// sequences with the Unicode replacement character (`"\u{FFFD}"`);
  /// otherwise, an ill-formed sequence causes this method to stop decoding
  /// and return `nil`.
  ///
  /// The following example calls this method with pointers to the contents of
  /// two different `CChar` arrays---the first with well-formed UTF-8 code
  /// unit sequences and the second with an ill-formed sequence at the end.
  ///
  ///     let validUTF8: [UInt8] = [67, 97, 102, 195, 169, 0]
  ///     validUTF8.withUnsafeBufferPointer { ptr in
  ///         let s = String.decodeCString(ptr.baseAddress,
  ///                                      as: UTF8.self,
  ///                                      repairingInvalidCodeUnits: true)
  ///         print(s)
  ///     }
  ///     // Prints "Optional((Café, false))"
  ///
  ///     let invalidUTF8: [UInt8] = [67, 97, 102, 195, 0]
  ///     invalidUTF8.withUnsafeBufferPointer { ptr in
  ///         let s = String.decodeCString(ptr.baseAddress,
  ///                                      as: UTF8.self,
  ///                                      repairingInvalidCodeUnits: true)
  ///         print(s)
  ///     }
  ///     // Prints "Optional((Caf�, true))"
  ///
  /// - Parameters:
  ///   - cString: A pointer to a null-terminated code sequence encoded in
  ///     `encoding`.
  ///   - encoding: The Unicode encoding of the data referenced by `cString`.
  ///   - isRepairing: Pass `true` to create a new string, even when the data
  ///     referenced by `cString` contains ill-formed sequences. Ill-formed
  ///     sequences are replaced with the Unicode replacement character
  ///     (`"\u{FFFD}"`). Pass `false` to interrupt the creation of the new
  ///     string if an ill-formed sequence is detected.
  /// - Returns: A tuple with the new string and a Boolean value that indicates
  ///   whether any repairs were made. If `isRepairing` is `false` and an
  ///   ill-formed sequence is detected, this method returns `nil`.
  ///
  /// - SeeAlso: `UnicodeCodec`
  public static func decodeCString<Encoding : _UnicodeEncoding>(
    _ cString: UnsafePointer<Encoding.CodeUnit>?,
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isRepairing: Bool = true)
      -> (result: String, repairsMade: Bool)? {

    guard let cString = cString else {
      return nil
    }
    var end = cString
    while end.pointee != 0 { end += 1 }
    let len = end - cString
    return _decodeCString(
      cString, as: encoding, length: len,
      repairingInvalidCodeUnits: isRepairing)
  }

}

/// From a non-`nil` `UnsafePointer` to a null-terminated string
/// with possibly-transient lifetime, create a null-terminated array of 'C' char.
/// Returns `nil` if passed a null pointer.
public func _persistCString(_ p: UnsafePointer<CChar>?) -> [CChar]? {
  guard let s = p else {
    return nil
  }
  let count = Int(_swift_stdlib_strlen(s))
  var result = [CChar](repeating: 0, count: count + 1)
  for i in 0..<count {
    result[i] = s[i]
  }
  return result
}

/// Creates a new string by copying the null-terminated data referenced by
/// the given pointer using the specified encoding.
///
/// This internal helper takes the string length as an argument.
func _decodeCString<Encoding : _UnicodeEncoding>(
  _ cString: UnsafePointer<Encoding.CodeUnit>,
  as encoding: Encoding.Type, length: Int,
  repairingInvalidCodeUnits isRepairing: Bool = true)
-> (result: String, repairsMade: Bool)? {

  let buffer = UnsafeBufferPointer<Encoding.CodeUnit>(
    start: cString, count: length)

  let (stringBuffer, hadError) = _StringBuffer.fromCodeUnits(
    buffer, encoding: encoding, repairIllFormedSequences: isRepairing)
  return stringBuffer.map {
    (result: String(_storage: $0), repairsMade: hadError)
  }
}

extension StringProtocol where Self : _SwiftStringView {
  /// Invokes the given closure on the contents of the string, represented as a
  /// pointer to a null-terminated sequence of UTF-8 code units.
  ///
  /// The `withCString(_:)` method ensures that the sequence's lifetime extends
  /// through the execution of `body`. The pointer argument to `body` is only
  /// valid for the lifetime of the closure. Do not escape it from the closure
  /// for later use.
  ///
  /// - Parameter body: A closure that takes a pointer to the string's UTF-8
  ///   code unit sequence as its sole argument. If the closure has a return
  ///   value, it is used as the return value of the `withCString(_:)` method.
  ///   The pointer argument is valid only for the duration of the closure's
  ///   execution.
  /// - Returns: The return value of the `body` closure, if any.
  @_inlineable
  public func withCString<Result>(
    _ body: (UnsafePointer<Int8>) throws -> Result
  ) rethrows -> Result {
    return try self._ephemeralString().utf8CString.withUnsafeBufferPointer {
      try body($0.baseAddress!)
    }
  }
}

extension String {
  @available(*, unavailable, message: "Please use String.init?(validatingUTF8:) instead. Note that it no longer accepts NULL as a valid input. Also consider using String(cString:), that will attempt to repair ill-formed code units.")
  public static func fromCString(_ cs: UnsafePointer<CChar>) -> String? {
    Builtin.unreachable()
  }

  @available(*, unavailable, message: "Please use String.init(cString:) instead. Note that it no longer accepts NULL as a valid input. See also String.decodeCString if you need more control.")
  public static func fromCStringRepairingIllFormedUTF8(
    _ cs: UnsafePointer<CChar>
  ) -> (String?, hadError: Bool) {
    Builtin.unreachable()
  }
}
