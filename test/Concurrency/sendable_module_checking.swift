// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -warn-concurrency %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift
// RUN: %target-swift-frontend -typecheck -disable-availability-checking -I %t 2>&1 %s | %FileCheck %s

// REQUIRES: concurrency

import StrictModule
import NonStrictModule

actor A {
  func f() -> [StrictStruct: NonStrictClass] { [:] }
}

func testA(a: A) async {
  _ = await a.f() // CHECK: warning: cannot call function returning non-sendable type '[StrictStruct : NonStrictClass]' across actors}}
  // CHECK-NOT: NonStrictClass
  // CHECK: note: struct 'StrictStruct' does not conform to the `Sendable` protocol
  // CHECK-NOT: NonStrictClass
}
