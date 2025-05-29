// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/variadic_generic_opaque_multifile_other.swift -target %target-swift-5.9-abi-triple
// RUN: %target-swift-frontend -emit-silgen %s %S/Inputs/variadic_generic_opaque_multifile_other.swift -target %target-swift-5.9-abi-triple

public func caller() {
  callee(1, 2, 3).f()
}
