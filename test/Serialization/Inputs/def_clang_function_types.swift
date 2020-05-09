import ctypes

public var has_fp_type: FunctionPointerReturningOpaqueTypedef?

public func use_inout<T>(arg: inout T) {}

@inlinable
public func use_fp_internally() {
  // This currently bypasses the safety checks we do in export-checking
  // because we don't check inlinable function bodies.  It's plausible
  // code, but more importantly it actually appears in the non-Darwin
  // Foundation code.
  var x: FunctionPointerReturningOpaqueTypedef2? = nil
  use_inout(arg: &x)
}
