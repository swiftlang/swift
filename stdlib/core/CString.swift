//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// CString Type
//===----------------------------------------------------------------------===//

// XXX FIXME: we need a clean memory management story here

/// An equivalent of nul-terminated `char *` in C.
///
/// `CString` behaves exactly like `char *`:
///
/// * it does not own the storage;
///
/// * it can contain a `NULL` pointer;
///
/// * it may not contain well-formed UTF-8.  Because of this, comparison
///   operators `<` and `==` on `CString` use `strcmp` instead of Unicode
///   comparison algorithms.
@public struct CString :
    _BuiltinExtendedGraphemeClusterLiteralConvertible,
    ExtendedGraphemeClusterLiteralConvertible,
    _BuiltinStringLiteralConvertible, StringLiteralConvertible,
    LogicValue {
  @public var _bytesPtr: UnsafePointer<UInt8>

  @public init(_ bytesPtr: UnsafePointer<UInt8>) {
    self._bytesPtr = bytesPtr
  }

  @public init(_ bytesPtr: UnsafePointer<CChar>) {
    self._bytesPtr = UnsafePointer<UInt8>(bytesPtr)
  }

  static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
    start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) -> CString {

    return _convertFromBuiltinStringLiteral(start, byteSize: byteSize,
                                            isASCII: isASCII)
  }

  @public static func convertFromExtendedGraphemeClusterLiteral(
    value: CString) -> CString {

    return convertFromStringLiteral(value)
  }

  static func _convertFromBuiltinStringLiteral(start: Builtin.RawPointer,
                                               byteSize: Builtin.Word,
                                               isASCII: Builtin.Int1) -> CString {
    return CString(UnsafePointer<CChar>(start))
  }

  @public static func convertFromStringLiteral(value: CString) -> CString {
    return value
  }

  @transparent
  @public var _isNull : Bool {
    return _bytesPtr._isNull
  }

  @transparent @public
  func getLogicValue() -> Bool {
    return !_isNull
  }

  /// From a non-`nil` `CString` with possibly-transient lifetime, create a
  /// nul-terminated array of 'C' char.
  /// Returns `nil` if the `CString` was created from a null pointer.
  @public func persist() -> [CChar]? {
    if !self {
      return .None
    }
    var length = _strlen(self)
    var result = [CChar](count: length + 1, repeatedValue: 0)
    for var i = 0; i < length; ++i {
      // FIXME: this will not compile on platforms where 'CChar' is unsigned.
      result[i] = _bytesPtr[i].asSigned()
    }
    return result
  }
}

extension CString : DebugPrintable {
  @public var debugDescription: String {
    let (optionalString, hadError) =
        String.fromCStringRepairingIllFormedUTF8(self)
    if let s = optionalString {
      return (hadError ? "<ill-formed UTF-8>" : "") + s.debugDescription
    }
    return "<null C string>"
  }
}

@asmname("strlen")
func _strlen(arg : CString) -> Int
@asmname("strcpy")
func _strcpy(dest: CString, src: CString) -> CString
@asmname("strcmp")
func _strcmp(dest: CString, src: CString) -> CInt

@transparent @public
func ==(lhs: CString, rhs: CString) -> Bool {
  if lhs._bytesPtr == rhs._bytesPtr { return true }
  return _strcmp(lhs, rhs) == 0
}

@transparent @public
func <(lhs: CString, rhs: CString) -> Bool {
  return _strcmp(lhs, rhs) < 0
}

extension CString : Equatable, Hashable, Comparable {
  @transparent @public
  var hashValue: Int {
    if let s = String.fromCStringRepairingIllFormedUTF8(self).0 {
      return s.hashValue
    }
    return 0
  }
}

extension String {
  /// Creates a new `String` by copying the nul-terminated UTF-8 data
  /// referenced by a `CString`.
  ///
  /// Returns `nil` if the `CString` is `NULL` or if it contains ill-formed
  /// UTF-8 code unit sequences.
  @public static func fromCString(cs: CString) -> String? {
    if cs._isNull {
      return .None
    }
    let len = Int(_strlen(cs))
    return String._fromCodeUnitSequence(UTF8.self,
        input: UnsafeArray(start: cs._bytesPtr, length: len))
  }

  /// Creates a new `String` by copying the nul-terminated UTF-8 data
  /// referenced by a `CString`.
  ///
  /// Returns `nil` if the `CString` is `NULL`.  If `CString` contains
  /// ill-formed UTF-8 code unit sequences, replaces them with replacement
  /// characters (U+FFFD).
  @public static func fromCStringRepairingIllFormedUTF8(cs: CString)
      -> (String?, hadError: Bool) {
    if cs._isNull {
      return (.None, hadError: false)
    }
    let len = Int(_strlen(cs))
    let (result, hadError) = String._fromCodeUnitSequenceWithRepair(UTF8.self,
        input: UnsafeArray(start: cs._bytesPtr, length: len))
    return (result, hadError: hadError)
  }
}

