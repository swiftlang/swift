/// A stdlib-internal protocol modeled by the intrinsic pointer types,
/// UnsafePointer, ConstUnsafePointer, and AutoreleasingUnsafePointer.
protocol _Pointer {
  /// The underlying raw pointer value.
  var value: Builtin.RawPointer { get }

  /// Construct a pointer from a raw value.
  init(_ value: Builtin.RawPointer)
}

/// Derive a pointer argument from a convertible pointer type.
@transparent
func _convertPointerToPointerArgument<
  FromPointer: _Pointer,
  ToPointer: _Pointer
>(from: FromPointer) -> ToPointer {
  return ToPointer(from.value)
}

/// Derive a pointer argument from an inout parameter.
///
/// This function is invoked with special writeback semantics; the lifetime
/// of the inout buffer is guaranteed for the duration of the call for which
/// this conversion is performed, rather than only for the duration of the
/// conversion call itself.
@transparent
func _convertInOutToPointerArgument<
  From,
  ToPointer: _Pointer
>(inout from: From) -> ToPointer {
  return ToPointer(Builtin.addressof(&from))
}

/// Derive a pointer argument from an inout array parameter.
@transparent
func _convertMutableArrayToPointerArgument<
  FromElement,
  ToPointer: _Pointer
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
  ToPointer: _Pointer
>(arr: Array<FromElement>) -> (AnyObject?, ToPointer) {
  let (owner: AnyObject?, raw) = arr._cPointerArgs()
  return (owner, ToPointer(raw))
}


