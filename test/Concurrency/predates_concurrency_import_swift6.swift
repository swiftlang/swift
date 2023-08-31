// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -warn-concurrency %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift

// RUN: %target-swift-frontend -swift-version 6 -I %t %s %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -swift-version 6 -I %t %s %s -emit-sil -o /dev/null -verify -enable-experimental-feature SendNonSendable

// REQUIRES: asserts

@preconcurrency import NonStrictModule
@preconcurrency import StrictModule

func acceptSendable<T: Sendable>(_: T) { }

@available(SwiftStdlib 5.1, *)
func test(ss: StrictStruct, ns: NonStrictClass) {
  acceptSendable(ss) // expected-warning{{type 'StrictStruct' does not conform to the 'Sendable' protocol}}
  acceptSendable(ns) // expected-warning{{type 'NonStrictClass' does not conform to the 'Sendable' protocol}}
}
