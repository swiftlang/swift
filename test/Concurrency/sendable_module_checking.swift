// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -warn-concurrency %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift

// We leave this as just type check since we are checking something that is cross module.
// RUN: %target-swift-frontend -typecheck -strict-concurrency=targeted -disable-availability-checking -I %t 2>&1 %s | %FileCheck %s

// REQUIRES: concurrency

import StrictModule
import NonStrictModule

actor A {
  func f() -> [StrictStruct: NonStrictClass] { [:] }
}

func testA(a: A) async {
  _ = await a.f() // CHECK: warning: cannot call function returning non-sendable type '[StrictStruct : NonStrictClass]' across actors}}
  // CHECK: note: struct 'StrictStruct' does not conform to the 'Sendable' protocol
  // CHECK: note: class 'NonStrictClass' does not conform to the 'Sendable' protocol
}

extension NonStrictStruct: @unchecked Sendable { }
