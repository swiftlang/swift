// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -warn-concurrency %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift
// RUN: %target-typecheck-verify-swift -disable-availability-checking -I %t

// REQUIRES: concurrency

import StrictModule
import NonStrictModule // expected-remark{{add '@preconcurrency' to suppress 'Sendable'-related warnings from module 'NonStrictModule'}}

actor A {
  func f() -> [StrictStruct: NonStrictClass] { [:] }
}

class NS { } // expected-note{{class 'NS' does not conform to the 'Sendable' protocol}}

struct MyType {
  var nsc: NonStrictClass
}

struct MyType2: Sendable {
  var nsc: NonStrictClass // expected-warning{{stored property 'nsc' of 'Sendable'-conforming struct 'MyType2' has non-sendable type 'NonStrictClass'}}
  var ns: NS // expected-warning{{stored property 'ns' of 'Sendable'-conforming struct 'MyType2' has non-sendable type 'NS'}}
}

func testA(ns: NS, mt: MyType, mt2: MyType2) async {
  Task {
    print(ns)
    print(mt) // no warning: MyType is Sendable because we suppressed NonStrictClass's warning
    print(mt2)
  }
}

extension NonStrictStruct: @unchecked Sendable { }
