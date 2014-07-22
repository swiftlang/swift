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

/// A stdlib-internal protocol modeled by the intrinsic pointer types,
/// UnsafeMutablePointer, UnsafePointer, and
/// AutoreleasingUnsafeMutablePointer.
protocol _PointerType {
  /// The underlying raw pointer value.
  var value: Builtin.RawPointer { get }

  /// Construct a pointer from a raw value.
  init(_ value: Builtin.RawPointer)
}

/// Derive a pointer argument from a convertible pointer type.
@transparent
func _convertPointerToPointerArgument<
  FromPointer: _PointerType,
  ToPointer: _PointerType
>(from: FromPointer) -> ToPointer {
  return ToPointer(from.value)
}

/// Derive a pointer argument from the address of an inout parameter.
@transparent
func _convertInOutToPointerArgument<
  ToPointer: _PointerType
>(from: Builtin.RawPointer) -> ToPointer {
  return ToPointer(from)
}

/// Derive a pointer argument from an inout array parameter.
@transparent
func _convertMutableArrayToPointerArgument<
  FromElement,
  ToPointer: _PointerType
>(inout a: Array<FromElement>) -> (AnyObject?, ToPointer) {
  // TODO: Putting a canary at the end of the array in checked builds might
  // be a good idea

  // Call reserve to force contiguous storage.
  a.reserveCapacity(0)
  _debugPrecondition(a._elementStorageIfContiguous != nil || a.count == 0)

  return (a._owner, ToPointer(a._elementStorageIfContiguous.value))
}

/// Derive a pointer argument from a value array parameter.
@transparent
func _convertConstArrayToPointerArgument<
  FromElement,
  ToPointer: _PointerType
>(arr: Array<FromElement>) -> (AnyObject?, ToPointer) {
  let (owner: AnyObject?, raw) = arr._cPointerArgs()
  return (owner, ToPointer(raw))
}

/// Derive a UTF-8 pointer argument from a value string parameter.
@transparent
func _convertConstStringToUTF8PointerArgument<
  ToPointer: _PointerType
>(str: String) -> (AnyObject?, ToPointer) {
  // Convert the UTF-8 representation to a null-terminated array.
  var utf8 = Array(str.utf8)
  utf8.append(0)
  // Extract the owner and pointer from the array.
  let (owner: AnyObject?, raw) = utf8._cPointerArgs()
  return (owner, ToPointer(raw))
}
