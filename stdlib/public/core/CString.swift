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

extension String {

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
    self = _decodeValidCString(cString, repair: true)
  }

  /// Creates a new string by copying the null-terminated UTF-8 data referenced
  /// by the given pointer.
  ///
  /// This is identical to init(cString: UnsafePointer<CChar> but operates on an
  /// unsigned sequence of bytes.
  public init(cString: UnsafePointer<UInt8>) {
    self = _decodeValidCString(cString, repair: true)
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
    guard let str = _decodeCString(cString, repair: false) else {
      return nil
    }
    self = str
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
  @_specialize(where Encoding == Unicode.UTF8)
  @_specialize(where Encoding == Unicode.UTF16)
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
  let count = Int(_stdlib_strlen(s))
  var result = [CChar](repeating: 0, count: count + 1)
  for i in 0..<count {
    result[i] = s[i]
  }
  return result
}

internal func _decodeValidCString(
  _ cString: UnsafePointer<Int8>, repair: Bool
) -> String {
  let len = UTF8._nullCodeUnitOffset(in: cString)
  return cString.withMemoryRebound(to: UInt8.self, capacity: len) {
    (ptr: UnsafePointer<UInt8>) -> String in
    let bufPtr = UnsafeBufferPointer(start: ptr, count: len)
    return String._fromWellFormedUTF8(bufPtr, repair: repair)
  }
}

internal func _decodeValidCString(
  _ cString: UnsafePointer<UInt8>, repair: Bool
) -> String {
  let len = UTF8._nullCodeUnitOffset(in: cString)
  let bufPtr = UnsafeBufferPointer(start: cString, count: len)
  return String._fromWellFormedUTF8(bufPtr, repair: repair)
}

internal func _decodeCString(
  _ cString: UnsafePointer<Int8>, repair: Bool
) -> String? {
  let len = UTF8._nullCodeUnitOffset(in: cString)
  return cString.withMemoryRebound(to: UInt8.self, capacity: len) {
    (ptr: UnsafePointer<UInt8>) -> String? in
    let bufPtr = UnsafeBufferPointer(start: ptr, count: len)
    return String._fromUTF8(bufPtr, repair: repair)
  }
}

internal func _decodeCString(
  _ cString: UnsafePointer<UInt8>, repair: Bool
) -> String? {
  let len = UTF8._nullCodeUnitOffset(in: cString)
  let bufPtr = UnsafeBufferPointer(start: cString, count: len)
  return String._fromUTF8(bufPtr, repair: repair)
}

/// Creates a new string by copying the null-terminated data referenced by
/// the given pointer using the specified encoding.
///
/// This internal helper takes the string length as an argument.
internal func _decodeCString<Encoding : _UnicodeEncoding>(
  _ cString: UnsafePointer<Encoding.CodeUnit>,
  as encoding: Encoding.Type, length: Int,
  repairingInvalidCodeUnits isRepairing: Bool = true)
-> (result: String, repairsMade: Bool)? {

  let buffer = UnsafeBufferPointer<Encoding.CodeUnit>(
    start: cString, count: length)

  let (guts, hadError) = _StringGuts.fromCodeUnits(
    buffer, encoding: encoding, repairIllFormedSequences: isRepairing)
  return guts.map { (result: String($0), repairsMade: hadError) }
}
