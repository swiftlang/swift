// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -swift-version 6 %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift

// RUN: %target-swift-frontend  -I %t %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency
// REQUIRES: asserts

@preconcurrency import NonStrictModule
@preconcurrency import StrictModule

@available(SwiftStdlib 5.1, *)
actor ActorWithDeinit {
  var ns: NonStrictClass = NonStrictClass()
  var ss: StrictStruct = StrictStruct()

  deinit {
    print(ns)
    print(ss) // expected-warning{{cannot access property 'ss' with a non-Sendable type 'StrictStruct' from nonisolated deinit}}
  }
}
