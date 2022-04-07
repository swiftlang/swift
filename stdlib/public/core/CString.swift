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
  /// - Parameter nullTerminatedUTF8: A pointer to a null-terminated UTF-8 code sequence.
  public init(cString nullTerminatedUTF8: UnsafePointer<CChar>) {
    let len = UTF8._nullCodeUnitOffset(in: nullTerminatedUTF8)
    let buffer = UnsafeBufferPointer(start: nullTerminatedUTF8, count: len)
    self = buffer.withMemoryRebound(to: UInt8.self) {
      String._fromUTF8Repairing($0).0
    }
  }

  @inlinable
  @_alwaysEmitIntoClient
  public init(cString nullTerminatedUTF8: [CChar]) {
    self = nullTerminatedUTF8.withUnsafeBufferPointer {
      $0.withMemoryRebound(to: UInt8.self, String.init(_checkingCString:))
    }
  }

  @_alwaysEmitIntoClient
  private init(_checkingCString bytes: UnsafeBufferPointer<UInt8>) {
    guard let length = bytes.firstIndex(of: 0) else {
      _preconditionFailure(
        "input of String.init(cString:) must be null-terminated"
      )
    }
    self = String._fromUTF8Repairing(
      UnsafeBufferPointer(
        start: bytes.baseAddress._unsafelyUnwrappedUnchecked,
        count: length
      )
    ).0
  }

  @inlinable
  @_alwaysEmitIntoClient
  @available(*, deprecated, message: "Use String(_ scalar: Unicode.Scalar)")
  public init(cString nullTerminatedUTF8: inout CChar) {
    guard nullTerminatedUTF8 == 0 else {
      _preconditionFailure(
        "input of String.init(cString:) must be null-terminated"
      )
    }
    self = ""
  }

  /// Creates a new string by copying the null-terminated UTF-8 data referenced
  /// by the given pointer.
  ///
  /// This is identical to `init(cString: UnsafePointer<CChar>)` but operates on
  /// an unsigned sequence of bytes.
  public init(cString nullTerminatedUTF8: UnsafePointer<UInt8>) {
    let len = UTF8._nullCodeUnitOffset(in: nullTerminatedUTF8)
    self = String._fromUTF8Repairing(
      UnsafeBufferPointer(start: nullTerminatedUTF8, count: len)).0
  }

  @inlinable
  @_alwaysEmitIntoClient
  public init(cString nullTerminatedUTF8: [UInt8]) {
    self = nullTerminatedUTF8.withUnsafeBufferPointer {
      String(_checkingCString: $0)
    }
  }

  @inlinable
  @_alwaysEmitIntoClient
  @available(*, deprecated, message: "Use a copy of the String argument")
  public init(cString nullTerminatedUTF8: String) {
    self = nullTerminatedUTF8.withCString(String.init(cString:))
  }

  @inlinable
  @_alwaysEmitIntoClient
  @available(*, deprecated, message: "Use String(_ scalar: Unicode.Scalar)")
  public init(cString nullTerminatedUTF8: inout UInt8) {
    guard nullTerminatedUTF8 == 0 else {
      _preconditionFailure(
        "input of String.init(cString:) must be null-terminated"
      )
    }
    self = ""
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
  ///     // Prints "Optional("Café")"
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
    guard let str = cString.withMemoryRebound(to: UInt8.self, capacity: len, {
      String._tryFromUTF8(UnsafeBufferPointer(start: $0, count: len))
    })
    else { return nil }

    self = str
  }

  @inlinable
  @_alwaysEmitIntoClient
  public init?(validatingUTF8 cString: [CChar]) {
    guard let length = cString.firstIndex(of: 0) else {
      _preconditionFailure(
        "input of String.init(validatingUTF8:) must be null-terminated"
      )
    }
    guard let string = cString.prefix(length).withUnsafeBufferPointer({
      $0.withMemoryRebound(to: UInt8.self, String._tryFromUTF8(_:))
    })
    else { return nil }

    self = string
  }

  @inlinable
  @_alwaysEmitIntoClient
  @available(*, deprecated, message: "Use a copy of the String argument")
  public init?(validatingUTF8 cString: String) {
    self = cString.withCString(String.init(cString:))
  }

  @inlinable
  @_alwaysEmitIntoClient
  @available(*, deprecated, message: "Use String(_ scalar: Unicode.Scalar)")
  public init?(validatingUTF8 cString: inout CChar) {
    guard cString == 0 else {
      _preconditionFailure(
        "input of String.init(validatingUTF8:) must be null-terminated"
      )
    }
    self = ""
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
  ///     // Prints "Optional((result: "Café", repairsMade: false))"
  ///
  ///     let invalidUTF8: [UInt8] = [67, 97, 102, 195, 0]
  ///     invalidUTF8.withUnsafeBufferPointer { ptr in
  ///         let s = String.decodeCString(ptr.baseAddress,
  ///                                      as: UTF8.self,
  ///                                      repairingInvalidCodeUnits: true)
  ///         print(s)
  ///     }
  ///     // Prints "Optional((result: "Caf�", repairsMade: true))"
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
  @inlinable // Fold away specializations
  public static func decodeCString<Encoding: _UnicodeEncoding>(
    _ cString: UnsafePointer<Encoding.CodeUnit>?,
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isRepairing: Bool = true
  ) -> (result: String, repairsMade: Bool)? {
    guard let cPtr = cString else { return nil }

    if _fastPath(encoding == Unicode.UTF8.self) {
      let len = UTF8._nullCodeUnitOffset(
        in: UnsafeRawPointer(cPtr).assumingMemoryBound(to: UInt8.self)
      )
      let bytes = UnsafeBufferPointer(start: cPtr, count: len)
      return bytes.withMemoryRebound(to: UInt8.self) { codeUnits in
        if isRepairing {
          return String._fromUTF8Repairing(codeUnits)
        }
        else if let str = String._tryFromUTF8(codeUnits) {
          return (str, false)
        }
        return nil
      }
    }

    var end = cPtr
    while end.pointee != 0 { end += 1 }
    let len = end - cPtr
    let codeUnits = UnsafeBufferPointer(start: cPtr, count: len)
    return String._fromCodeUnits(
      codeUnits, encoding: encoding, repair: isRepairing)
  }

  @_specialize(where Encoding == Unicode.UTF8)
  @_specialize(where Encoding == Unicode.UTF16)
  @inlinable // Fold away specializations
  @_alwaysEmitIntoClient
  public static func decodeCString<Encoding: _UnicodeEncoding>(
    _ cString: [Encoding.CodeUnit],
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isRepairing: Bool = true
  ) -> (result: String, repairsMade: Bool)? {
    guard let length = cString.firstIndex(of: 0) else {
      _preconditionFailure(
        "input of decodeCString(_:as:repairingInvalidCodeUnits:) must be null-terminated"
      )
    }

    if _fastPath(encoding == Unicode.UTF8.self) {
      return cString.prefix(length).withUnsafeBufferPointer {
        buffer -> (result: String, repairsMade: Bool)? in
        return buffer.withMemoryRebound(to: UInt8.self) { codeUnits in
          if isRepairing {
            return String._fromUTF8Repairing(codeUnits)
          }
          else if let str = String._tryFromUTF8(codeUnits) {
            return (str, false)
          }
          return nil
        }
      }
    }

    return cString.prefix(length).withUnsafeBufferPointer {
      buf -> (result: String, repairsMade: Bool)? in
      String._fromCodeUnits(buf, encoding: encoding, repair: isRepairing)
    }
  }

  @_specialize(where Encoding == Unicode.UTF8)
  @_specialize(where Encoding == Unicode.UTF16)
  @inlinable
  @_alwaysEmitIntoClient
  @available(*, deprecated, message: "Use a copy of the String argument")
  public static func decodeCString<Encoding: _UnicodeEncoding>(
    _ cString: String,
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isRepairing: Bool = true
  ) -> (result: String, repairsMade: Bool)? {
    return cString.withCString(encodedAs: encoding) {
      String.decodeCString(
        $0, as: encoding, repairingInvalidCodeUnits: isRepairing
      )
    }
  }

  @_specialize(where Encoding == Unicode.UTF8)
  @_specialize(where Encoding == Unicode.UTF16)
  @inlinable
  @_alwaysEmitIntoClient
  @available(*, deprecated, message: "Use String(_ scalar: Unicode.Scalar)")
  public static func decodeCString<Encoding: _UnicodeEncoding>(
    _ cString: inout Encoding.CodeUnit,
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isRepairing: Bool = true
  ) -> (result: String, repairsMade: Bool)? {
    guard cString == 0 else {
      _preconditionFailure(
        "input of decodeCString(_:as:repairingInvalidCodeUnits:) must be null-terminated"
      )
    }
    return ("", false)
  }

  /// Creates a string from the null-terminated sequence of bytes at the given
  /// pointer.
  ///
  /// - Parameters:
  ///   - nullTerminatedCodeUnits: A pointer to a sequence of contiguous code
  ///     units in the encoding specified in `sourceEncoding`, ending just
  ///     before the first zero code unit.
  ///   - sourceEncoding: The encoding in which the code units should be
  ///     interpreted.
  @_specialize(where Encoding == Unicode.UTF8)
  @_specialize(where Encoding == Unicode.UTF16)
  @inlinable // Fold away specializations
  public init<Encoding: Unicode.Encoding>(
    decodingCString nullTerminatedCodeUnits: UnsafePointer<Encoding.CodeUnit>,
    as sourceEncoding: Encoding.Type
  ) {
    self = String.decodeCString(nullTerminatedCodeUnits, as: sourceEncoding)!.0
  }

  @_specialize(where Encoding == Unicode.UTF8)
  @_specialize(where Encoding == Unicode.UTF16)
  @inlinable // Fold away specializations
  @_alwaysEmitIntoClient
  public init<Encoding: Unicode.Encoding>(
    decodingCString nullTerminatedCodeUnits: [Encoding.CodeUnit],
    as sourceEncoding: Encoding.Type
  ) {
    self = String.decodeCString(nullTerminatedCodeUnits, as: sourceEncoding)!.0
  }

  @_specialize(where Encoding == Unicode.UTF8)
  @_specialize(where Encoding == Unicode.UTF16)
  @inlinable
  @_alwaysEmitIntoClient
  @available(*, deprecated, message: "Use a copy of the String argument")
  public init<Encoding: _UnicodeEncoding>(
    decodingCString nullTerminatedCodeUnits: String,
    as sourceEncoding: Encoding.Type
  ) {
    self = nullTerminatedCodeUnits.withCString(encodedAs: sourceEncoding) {
      String(decodingCString: $0, as: sourceEncoding.self)
    }
  }

  @_specialize(where Encoding == Unicode.UTF8)
  @_specialize(where Encoding == Unicode.UTF16)
  @inlinable // Fold away specializations
  @_alwaysEmitIntoClient
  @available(*, deprecated, message: "Use String(_ scalar: Unicode.Scalar)")
  public init<Encoding: Unicode.Encoding>(
    decodingCString nullTerminatedCodeUnits: inout Encoding.CodeUnit,
    as sourceEncoding: Encoding.Type
  ) {
    guard nullTerminatedCodeUnits == 0 else {
      _preconditionFailure(
        "input of String.init(decodingCString:as:) must be null-terminated"
      )
    }
    self = ""
  }
}

extension UnsafePointer where Pointee == UInt8 {
  @inlinable
  internal var _asCChar: UnsafePointer<CChar> {
    @inline(__always) get {
      return UnsafeRawPointer(self).assumingMemoryBound(to: CChar.self)
    }
  }
}
extension UnsafePointer where Pointee == CChar {
  @inlinable
  internal var _asUInt8: UnsafePointer<UInt8> {
    @inline(__always) get {
      return UnsafeRawPointer(self).assumingMemoryBound(to: UInt8.self)
    }
  }
}

