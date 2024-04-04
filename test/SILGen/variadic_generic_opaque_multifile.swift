// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/variadic_generic_opaque_multifile_other.swift -disable-availability-checking
// RUN: %target-swift-frontend -emit-silgen %s %S/Inputs/variadic_generic_opaque_multifile_other.swift -disable-availability-checking

public func caller() {
  callee(1, 2, 3).f()
}
