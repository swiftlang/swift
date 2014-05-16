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

struct CString :
    _BuiltinExtendedGraphemeClusterLiteralConvertible,
    ExtendedGraphemeClusterLiteralConvertible,
    _BuiltinStringLiteralConvertible, StringLiteralConvertible {
  var _bytesPtr : UnsafePointer<UInt8>

  @transparent
  init(_ _bytesPtr : UnsafePointer<UInt8>) {
    self._bytesPtr = _bytesPtr
  }

  static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
    start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) -> CString {

    return _convertFromBuiltinStringLiteral(start, byteSize: byteSize,
                                            isASCII: isASCII)
  }

  static func convertFromExtendedGraphemeClusterLiteral(
    value: CString) -> CString {

    return convertFromStringLiteral(value)
  }

  static func _convertFromBuiltinStringLiteral(start: Builtin.RawPointer,
                                               byteSize: Builtin.Word,
                                               isASCII: Builtin.Int1) -> CString {
    return CString(UnsafePointer(start))
  }

  static func convertFromStringLiteral(value: CString) -> CString {
    return value
  }

  func isNull() -> Bool {
    return _bytesPtr.isNull()
  }

  /// \brief From a CString with possibly-transient lifetime, create a
  /// nul-terminated array of 'C' char.
  func persist() -> CChar[] {
    var length = _strlen(self)
    var result = new CChar[length + 1]
    for var i = 0; i < length; ++i {
      result[i] = CChar(_bytesPtr[i])
    }
    return result
  }
}

extension CString : DebugPrintable {
  var debugDescription: String {
    if isNull() {
      return "<null C string>"
    }    
    return String.fromCString(self).debugDescription
  }
}

@asmname("strlen")
func _strlen(arg : CString) -> Int
@asmname("strcpy")
func _strcpy(dest: CString, src: CString) -> CString
@asmname("strcmp")
func _strcmp(dest: CString, src: CString) -> Int

@transparent
func ==(lhs: CString, rhs: CString) -> Bool {
  if lhs._bytesPtr == rhs._bytesPtr { return true }
  return _strcmp(lhs, rhs) == 0
}

@transparent
func <(lhs: CString, rhs: CString) -> Bool {
  return _strcmp(lhs, rhs) < 0
}

extension CString : Equatable, Hashable, Comparable {
  @transparent
  var hashValue: Int {
    return String.fromCString(self).hashValue
  }
}

extension String {
  /// Creates a new String by copying the null-terminated data referenced by
  /// a CString.
  static func fromCString(cs: CString) -> String {
    var len = Int(_strlen(cs))
    return String(UTF8.self, 
                  input: UnsafeArray(start: cs._bytesPtr, length: len))
  }

  static func fromCString(up: UnsafePointer<CChar>) -> String {
    return fromCString(CString(UnsafePointer<UInt8>(up)))
  }
}
