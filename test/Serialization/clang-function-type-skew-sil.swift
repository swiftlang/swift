// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Library.swift \
// RUN:   -module-name Library -o %t/Library.swiftmodule
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t \
// RUN:   -use-clang-function-types -o /dev/null

// A client can request Clang function types when a dependency was built
// without them. Reconstruct a dependency's derivable C function types while
// deserializing its serialized SIL body.

//--- Library.swift
public typealias Callback = @convention(c) (CInt) -> CInt

@inlinable
public func invoke(_ function: Callback, _ value: CInt) -> CInt {
  return function(value)
}

//--- Client.swift
import Library

public func call(_ function: Callback, _ value: CInt) -> CInt {
  return invoke(function, value)
}
