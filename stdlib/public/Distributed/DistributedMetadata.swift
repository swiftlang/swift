//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// Get the parameter count from a mangled method name.
///
/// - Returns: May return a negative number to signal a decoding error.
@available(SwiftStdlib 5.6, *)
public // SPI _Distributed
func _getParameterCount(mangledMethodName name: String) -> Int32 {
  let nameUTF8 = Array(name.utf8)
  return nameUTF8.withUnsafeBufferPointer { nameUTF8 in
    return __getParameterCount(
        nameUTF8.baseAddress!, UInt(nameUTF8.endIndex))
  }
}

@available(SwiftStdlib 5.6, *)
@_silgen_name("swift_func_getParameterCount")
public // SPI _Distributed
func __getParameterCount(
    _ typeNameStart: UnsafePointer<UInt8>,
    _ typeNameLength: UInt
) -> Int32

/// Write the Metadata of all the mangled methods name's
/// parameters into the provided buffer.
///
/// - Returns: the actual number of types written,
///            or negative value to signify an error
@available(SwiftStdlib 5.6, *)
public // SPI _Distributed
func _getParameterTypeInfo(
    mangledMethodName name: String,
    into typesBuffer: Builtin.RawPointer, length typesLength: Int
) -> Int32 {
  let nameUTF8 = Array(name.utf8)
  return nameUTF8.withUnsafeBufferPointer { nameUTF8 in
    return __getParameterTypeInfo(
        nameUTF8.baseAddress!, UInt(nameUTF8.endIndex),
        typesBuffer, typesLength)
  }
}

/// - Returns: the actual number of types written,
///             or a negative value to signal decoding error.
@available(SwiftStdlib 5.6, *)
@_silgen_name("swift_func_getParameterTypeInfo")
public // SPI _Distributed
func __getParameterTypeInfo(
    _ typeNameStart: UnsafePointer<UInt8>, _ typeNameLength: UInt,
    _ types: Builtin.RawPointer, _ typesLength: Int
) -> Int32

@available(SwiftStdlib 5.6, *)
public // SPI _Distributed
func _getReturnTypeInfo(mangledMethodName name: String) -> Any.Type? {
  let nameUTF8 = Array(name.utf8)
  return nameUTF8.withUnsafeBufferPointer { nameUTF8 in
    return __getReturnTypeInfo(nameUTF8.baseAddress!, UInt(nameUTF8.endIndex))
  }
}

@available(SwiftStdlib 5.6, *)
@_silgen_name("swift_func_getReturnTypeInfo")
public // SPI _Distributed
func __getReturnTypeInfo(
    _ typeNameStart: UnsafePointer<UInt8>,
    _ typeNameLength: UInt
) -> Any.Type?
