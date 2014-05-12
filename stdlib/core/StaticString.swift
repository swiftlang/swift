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

/// \brief An extremely simple string designed to represent something
/// "statically knowable".

// Implementation Note: Because StaticString is used in the
// implementation of assert() and fatal(), we keep it extremely close
// to the bare metal.  In particular, because we use only Builtin
// types, we are guaranteed that no assertions are involved in its
// construction.  This feature is crucial for preventing infinite
// recursion even in non-asserting cases.
struct StaticString
  : _BuiltinExtendedGraphemeClusterLiteralConvertible,
    ExtendedGraphemeClusterLiteralConvertible,
    _BuiltinStringLiteralConvertible, StringLiteralConvertible {
  var start: Builtin.RawPointer
  var byteSize: Builtin.Word
  var isASCII: Builtin.Int1

  init() {
    self = ""
  }
  
  init(
    start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1
  ) {
    self.start = start
    self.byteSize = byteSize
    self.isASCII = isASCII
  }

  static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
    start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) -> StaticString {

    return _convertFromBuiltinStringLiteral(
        start, byteSize: byteSize, isASCII: isASCII)
  }

  static func convertFromExtendedGraphemeClusterLiteral(
      value: StaticString) -> StaticString {
    return value
  }

  static func _convertFromBuiltinStringLiteral(
    start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1
  ) -> StaticString {
    return StaticString(start: start, byteSize: byteSize, isASCII: isASCII)
  }
  
  static func convertFromStringLiteral(value: StaticString) -> StaticString {
    return value
  }
}
