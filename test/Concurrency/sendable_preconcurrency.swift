// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -warn-concurrency %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift
// RUN: %target-typecheck-verify-swift -disable-availability-checking -I %t

// REQUIRES: concurrency

import StrictModule // no remark: we never recommend @preconcurrency due to an explicitly non-Sendable (via -warn-concurrency) type
@preconcurrency import NonStrictModule

actor A {
  func f() -> [StrictStruct: NonStrictClass] { [:] }
}

class NS { } // expected-note 2{{class 'NS' does not conform to the 'Sendable' protocol}}

struct MyType {
  var nsc: NonStrictClass
}

struct MyType2: Sendable {
  var nsc: NonStrictClass // no warning; @preconcurrency suppressed it
  var ns: NS // expected-warning{{stored property 'ns' of 'Sendable'-conforming struct 'MyType2' has non-sendable type 'NS'}}
}

struct MyType3 {
  var nsc: NonStrictClass
}

func testA(ns: NS, mt: MyType, mt2: MyType2, mt3: MyType3, sc: StrictClass, nsc: NonStrictClass) async {
  Task {
    print(ns) // expected-warning{{capture of 'ns' with non-sendable type 'NS' in a `@Sendable` closure}}
    print(mt) // no warning: MyType is Sendable because we suppressed NonStrictClass's warning
    print(mt2)
    print(mt3)
    print(sc) // expected-warning {{capture of 'sc' with non-sendable type 'StrictClass' in a `@Sendable` closure}}
    print(nsc)
  }
}

extension NonStrictStruct: @unchecked Sendable { }
