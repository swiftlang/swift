// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -strict-concurrency=complete %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift

// RUN: %target-swift-frontend -c -strict-concurrency=complete -disable-availability-checking -I %t 2>&1 %s | %FileCheck %s

// REQUIRES: concurrency

import StrictModule
import NonStrictModule

actor A {
  func f() -> [StrictStruct: NonStrictClass] { [:] }
}

func testA(a: A) async {
  _ = await a.f()
  // CHECK: warning: non-Sendable '[StrictStruct : NonStrictClass]'-typed result can not be returned from actor-isolated instance method 'f()' to nonisolated context; this is an error in the Swift 6 language mode
  // CHECK: note: note: generic struct 'Dictionary' does not conform to the 'Sendable' protocol
}

extension NonStrictStruct: @unchecked Sendable { }
