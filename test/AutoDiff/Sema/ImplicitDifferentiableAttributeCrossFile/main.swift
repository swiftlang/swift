// Test missing protocol requirement `@differentiable` attribute errors for
// non-public protocol witnesses, when the protocol conformance is declared in a
// separate file from witnesses.
//
// Implicit `@differentiable` attributes cannot be generated for protocol
// witnesses when the conformance is declared from a separate file from the
// witness. Otherwise, compilation of the file containing the conformance
// creates references to external symbols for implicit `@differentiable`
// attributes, even though no such symbols exist.
//
// Context: https://github.com/apple/swift/pull/29771#issuecomment-585059721

// Note: `swiftc main.swift other_file.swift` runs three commands:
// - `swiftc -frontend -primary-file main.swift other_file.swift -o ...`
// - `swiftc -frontend main.swift -primary-file other_file.swift -o ...`
// - `/usr/bin/ld ...`
//
// `%target-build-swift` performs `swiftc main.swift other_file.swift`, so it is expected to fail (hence `not`).
// `swiftc -frontend -primary-file main.swift other_file.swift` should fail, so `-verify` is needed.
// `swiftc -frontend main.swift -primary-file other_file.swift` should succeed, so no need for `-verify`.

// RUN: %target-swift-frontend -c -verify -primary-file %s %S/Inputs/other_file.swift
// RUN: %target-swift-frontend -c %s -primary-file %S/Inputs/other_file.swift
// RUN: not %target-build-swift %s %S/Inputs/other_file.swift

import _Differentiation

// Error: conformance is in different file than witnesses.
// expected-error@+2 {{type 'ConformingStruct' does not conform to protocol 'Protocol1'}}
// expected-note@+1 {{add stubs for conformance}}
extension ConformingStruct: Protocol1 {}

// No error: conformance is in same file as witnesses.
extension ConformingStruct: Protocol2 {
  func internalMethod4(_ x: Float) -> Float {
    x
  }
}

public final class ConformingStructWithSupersetAttr: Protocol2 {}

// rdar://70348904: Witness mismatch failure when a matching witness with a *superset* `@differentiable`
// attribute is specified.
// 
// Note that public witnesses are required to explicitly specify `@differentiable` attributes except
// those w.r.t. parameters that have already been covered by an existing `@differentiable` attribute.
extension ConformingStructWithSupersetAttr {
  // @differentiable(reverse, wrt: self) // Omitting this is okay.
  @differentiable(reverse)
  public func internalMethod4(_ x: Float) -> Float { x }
}
