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
    let len = UTF8._nullCodeUnitOffset(in: cString)
    self = String._fromUTF8Repairing(
      UnsafeBufferPointer(start: cString._asUInt8, count: len)).0
  }

  /// Creates a new string by copying the null-terminated UTF-8 data referenced
  /// by the given pointer.
  ///
  /// This is identical to init(cString: UnsafePointer<CChar> but operates on an
  /// unsigned sequence of bytes.
  public init(cString: UnsafePointer<UInt8>) {
    let len = UTF8._nullCodeUnitOffset(in: cString)
    self = String._fromUTF8Repairing(
      UnsafeBufferPointer(start: cString, count: len)).0
  }

  /// Creates a new String with the specified capacity in UTF-8 code units then
  /// calls the given closure with a buffer covering the String's uninitialized
  /// memory.
  ///
  /// The closure should return the number of code units that are initialized,
  /// or nil if it was unable to initialize the buffer (for example if the
  /// requested capacity ended up being too small for the data).
  ///
  /// This initializer does not try to repair ill-formed UTF-8 code unit
  /// sequences. If any are found, the result of the initializer is `nil`.
  ///
  /// The following example uses this initializer with the contents of two
  /// different `CChar` arrays---the first with well-formed UTF-8 code unit
  /// sequences and the second with an ill-formed sequence at the end.
  ///
  ///     let validUTF8: [CChar] = [67, 97, 102, -61, -87, 0]
  ///     let s = String(unsafeUninitializedCapacity: validUTF8.count,
  ///                    initializingValidatingUTF8With: { (ptr, count) in
  ///         ptr.initializeFrom(validUTF8)
  ///         count = validUTF8.count
  ///     })
  ///     // Prints "Optional(Café)"
  ///
  ///     let invalidUTF8: [CChar] = [67, 97, 102, -61, 0]
  ///     let s = String(unsafeUninitializedCapacity: invalidUTF8.count,
  ///                    initializingValidatingUTF8With: { (ptr, count) in
  ///         ptr.initializeFrom(invalidUTF8)
  ///         count = invalidUTF8.count
  ///     })
  ///     // Prints "nil"
  ///
  /// - Parameters:
  ///   - capacity: The number of UTF-8 code units worth of memory to allocate
  ///       for the String.
  ///   - initializer: A closure that initializes elements and sets the count of
  ///       the new String
  ///     - Parameters:
  ///       - buffer: A buffer covering uninitialized memory with room for the
  ///           specified number of UTF-8 code units.
  ///       - initializedCount: Set this to the number of elements in `buffer`
  ///           that were actually initialized by the `initializer`
  @inlinable @inline(__always)
  public init?(
    unsafeUninitializedCapacity capacity: Int,
    initializingRepairingUTF8With initializer: (
      _ buffer: UnsafeMutableBufferPointer<UInt8>,
      _ initializedCount: inout Int
    ) throws -> Void
  ) rethrows {
    var utf8Smol: _SmallString? = nil
    if capacity <= _SmallString.capacity {
      let smol = try _SmallString(initializingUTF8With: initializer)
      // Fast case where we fit in a _SmallString and don't need UTF8 validation
      if smol.isASCII {
        self = String(_StringGuts(smol))
        return
      }
      utf8Smol = smol
    }

    try self.init(_unsafeLargeUninitializedCapacity: capacity,
                  nonASCIISmallString: utf8Smol,
                  initializingRepairingUTF8With: initializer)
  }

  @_effects(releasenone)
  @usableFromInline
  internal init?(
    _unsafeLargeUninitializedCapacity capacity: Int,
    nonASCIISmallString: _SmallString?,
    initializingRepairingUTF8With initializer: (
      _ buffer: UnsafeMutableBufferPointer<UInt8>,
      _ initializedCount: inout Int
    ) throws -> Void
  ) rethrows {
    if let smol = nonASCIISmallString {
      //We succeeded in making a _SmallString, but may need repair UTF8
      self = smol.withUTF8 { String._fromUTF8Repairing($0).result }
      return
    }
    _internalInvariant(capacity > _SmallString.capacity)
    guard let storage = try __StringStorage.create(
      unsafeUninitializedCapacity: capacity,
      initializingRepairingUTF8With: initializer
    ) else {
      return nil
    }
    
    self = storage.asString
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
    guard let str = String._tryFromUTF8(
      UnsafeBufferPointer(start: cString._asUInt8, count: len))
    else { return nil }

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
  @inlinable // Fold away specializations
  public static func decodeCString<Encoding : _UnicodeEncoding>(
    _ cString: UnsafePointer<Encoding.CodeUnit>?,
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isRepairing: Bool = true
  ) -> (result: String, repairsMade: Bool)? {
    guard let cPtr = cString else { return nil }

    if _fastPath(encoding == Unicode.UTF8.self) {
      let ptr = UnsafeRawPointer(cPtr).assumingMemoryBound(to: UInt8.self)
      let len = UTF8._nullCodeUnitOffset(in: ptr)
      let codeUnits = UnsafeBufferPointer(start: ptr, count: len)
      if isRepairing {
        return String._fromUTF8Repairing(codeUnits)
      } else {
        guard let str = String._tryFromUTF8(codeUnits) else { return nil }
        return (str, false)
      }
    }

    var end = cPtr
    while end.pointee != 0 { end += 1 }
    let len = end - cPtr
    let codeUnits = UnsafeBufferPointer(start: cPtr, count: len)
    return String._fromCodeUnits(
      codeUnits, encoding: encoding, repair: isRepairing)
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
    decodingCString ptr: UnsafePointer<Encoding.CodeUnit>,
    as sourceEncoding: Encoding.Type
  ) {
    self = String.decodeCString(ptr, as: sourceEncoding)!.0
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

