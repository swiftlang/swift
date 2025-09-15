// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -swift-version 6 %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift

// RUN: %target-swift-frontend -swift-version 6 -I %t %s -emit-sil -o /dev/null -verify -parse-as-library

@preconcurrency import NonStrictModule
@preconcurrency import StrictModule

func acceptSendable<T: Sendable>(_: T) { }

@available(SwiftStdlib 5.1, *)
func test(ss: StrictStruct, ns: NonStrictClass) {
  acceptSendable(ss) // expected-warning{{type 'StrictStruct' does not conform to the 'Sendable' protocol}}
  acceptSendable(ns)
}

let nonStrictGlobal = NonStrictClass()
let strictGlobal = StrictStruct() // expected-warning{{let 'strictGlobal' is not concurrency-safe because non-'Sendable' type 'StrictStruct' may have shared mutable state}}
// expected-note@-1{{add '@MainActor' to make let 'strictGlobal' part of global actor 'MainActor'}}
// expected-note@-2{{disable concurrency-safety checks if accesses are protected by an external synchronization mechanism}}

extension NonStrictClass {
  @Sendable func f() { }
}

extension StrictStruct {
  @Sendable func f() { } // expected-warning{{instance method of non-Sendable type 'StrictStruct' cannot be marked as '@Sendable'}}
}
