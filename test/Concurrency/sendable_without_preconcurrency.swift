// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -swift-version 6 %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift
// RUN: %target-swift-frontend -strict-concurrency=targeted -disable-availability-checking -I %t -verify %s -o /dev/null -emit-sil
// RUN: %target-swift-frontend -strict-concurrency=complete -disable-availability-checking -I %t -verify %s -o /dev/null -emit-sil -verify-additional-prefix complete-
// RUN: %target-swift-frontend -strict-concurrency=complete -disable-availability-checking -I %t -verify %s -o /dev/null -emit-sil -verify-additional-prefix complete- -enable-experimental-feature SendNonSendable

// REQUIRES: concurrency
// REQUIRES: asserts

import StrictModule // no remark: we never recommend @preconcurrency due to an explicitly non-Sendable (via -warn-concurrency) type
import NonStrictModule

actor A {
  func f() -> [StrictStruct: NonStrictClass] { [:] }
}

class NS { } // expected-note{{class 'NS' does not conform to the 'Sendable' protocol}}

struct MyType { // expected-complete-note {{consider making struct 'MyType' conform to the 'Sendable' protocol}}
  var nsc: NonStrictClass
}

struct MyType2 { // expected-note{{consider making struct 'MyType2' conform to the 'Sendable' protocol}}
  var nsc: NonStrictClass
  var ns: NS
}

func testA(ns: NS, mt: MyType, mt2: MyType2, sc: StrictClass, nsc: NonStrictClass) async {
  Task {
    print(ns) // expected-warning{{capture of 'ns' with non-sendable type 'NS' in a `@Sendable` closure}}
    print(mt) // no warning by default: MyType is Sendable because we suppressed NonStrictClass's warning
    // expected-complete-warning @-1 {{capture of 'mt' with non-sendable type 'MyType' in a `@Sendable` closure}}
    print(mt2) // expected-warning{{capture of 'mt2' with non-sendable type 'MyType2' in a `@Sendable` closure}}
    print(sc) // expected-warning{{capture of 'sc' with non-sendable type 'StrictClass' in a `@Sendable` closure}}
  }
}

extension NonStrictStruct: @unchecked Sendable { }

class StrictSubclass: StrictClass {
  override func send(_ body: () -> ()) {}
  override func dontSend(_ body: () -> ()) {}
}

struct StrictConformer: StrictProtocol {
  func send(_ body: () -> Void) {}
  func dontSend(_ body: () -> Void) {}
}

class NonStrictSubclass: NonStrictClass {
  override func send(_ body: () -> ()) {}
  override func dontSend(_ body: () -> ()) {}
}

struct NonStrictConformer: NonStrictProtocol {
  func send(_ body: () -> Void) {}
  func dontSend(_ body: () -> Void) {}
}
