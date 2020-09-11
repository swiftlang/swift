// Test missing protocol requirement `@differentiable` attribute errors for
// non-public protocol witnesses, when the protocol conformance is declared in a
// separate file from witnesses.
//
// Implicit `@differentiable` attributes cannot be generated for protocol
// witnesses when the conformance is declared from a separate file from the
// witness. Otherwise, compilation of the file containing the conformance
// creates external references to symbols for implicit `@differentiable`
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

// Error: conformance is in different file than witnesses.
// expected-error @+1 {{type 'ConformingStruct' does not conform to protocol 'Protocol'}}
extension ConformingStruct: Protocol {}

// No error: conformance is in same file as witnesses.
extension ConformingStruct: Protocol2 {
  func internalMethod4(_ x: Float) -> Float {
    x
  }
}
