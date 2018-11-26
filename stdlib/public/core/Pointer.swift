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

/// A stdlib-internal protocol modeled by the intrinsic pointer types,
/// UnsafeMutablePointer, UnsafePointer, and
/// AutoreleasingUnsafeMutablePointer.
public protocol _Pointer {
  /// The underlying raw pointer value.
  var _rawValue: Builtin.RawPointer { get }

  /// Creates a pointer from a raw value.
  init(_ _rawValue: Builtin.RawPointer)
}

/// Derive a pointer argument from a convertible pointer type.
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // COMPILER_INTRINSIC
func _convertPointerToPointerArgument<
  FromPointer : _Pointer,
  ToPointer : _Pointer
>(_ from: FromPointer) -> ToPointer {
  return ToPointer(from._rawValue)
}

/// Derive a pointer argument from the address of an inout parameter.
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // COMPILER_INTRINSIC
func _convertInOutToPointerArgument<
  ToPointer : _Pointer
>(_ from: Builtin.RawPointer) -> ToPointer {
  return ToPointer(from)
}

/// Derive a pointer argument from a value array parameter.
///
/// This always produces a non-null pointer, even if the array doesn't have any
/// storage.
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // COMPILER_INTRINSIC
func _convertConstArrayToPointerArgument<
  FromElement,
  ToPointer: _Pointer
>(_ arr: [FromElement]) -> (AnyObject?, ToPointer) {
  let (owner, opaquePointer) = arr._cPointerArgs()

  let validPointer: ToPointer
  if let addr = opaquePointer {
    validPointer = ToPointer(addr._rawValue)
  } else {
    let lastAlignedValue = ~(MemoryLayout<FromElement>.alignment - 1)
    let lastAlignedPointer = UnsafeRawPointer(bitPattern: lastAlignedValue)!
    validPointer = ToPointer(lastAlignedPointer._rawValue)
  }
  return (owner, validPointer)
}

/// Derive a pointer argument from an inout array parameter.
///
/// This always produces a non-null pointer, even if the array's length is 0.
@inlinable // FIXME(sil-serialize-all)
@_transparent
public // COMPILER_INTRINSIC
func _convertMutableArrayToPointerArgument<
  FromElement,
  ToPointer : _Pointer
>(_ a: inout [FromElement]) -> (AnyObject?, ToPointer) {
  // TODO: Putting a canary at the end of the array in checked builds might
  // be a good idea

  // Call reserve to force contiguous storage.
  a.reserveCapacity(0)
  _debugPrecondition(a._baseAddressIfContiguous != nil || a.isEmpty)

  return _convertConstArrayToPointerArgument(a)
}

/// Derive a UTF-8 pointer argument from a value string parameter.
@inlinable // FIXME(sil-serialize-all)
public // COMPILER_INTRINSIC
func _convertConstStringToUTF8PointerArgument<
  ToPointer : _Pointer
>(_ str: String) -> (AnyObject?, ToPointer) {
  let utf8 = Array(str.utf8CString)
  return _convertConstArrayToPointerArgument(utf8)
}
