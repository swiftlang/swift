// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -swift-version 6 %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift
// RUN: %target-swift-frontend -strict-concurrency=minimal -disable-availability-checking -I %t %s -verify -emit-sil -o /dev/null
// RUN: %target-swift-frontend -strict-concurrency=targeted -verify-additional-prefix targeted-complete- -disable-availability-checking -I %t %s -verify -emit-sil -o /dev/null
// RUN: %target-swift-frontend -strict-concurrency=complete -verify-additional-prefix targeted-complete- -verify-additional-prefix complete- -disable-availability-checking -I %t %s -verify -emit-sil -o /dev/null -disable-region-based-isolation-with-strict-concurrency
// RUN: %target-swift-frontend -strict-concurrency=complete -verify-additional-prefix targeted-complete- -verify-additional-prefix complete- -disable-availability-checking -I %t %s -verify -emit-sil -o /dev/null

// REQUIRES: concurrency

import StrictModule // no remark: we never recommend @preconcurrency due to an explicitly non-Sendable (via -strict-concurrency=complete) type
import NonStrictModule // expected-warning{{add '@preconcurrency' to suppress 'Sendable'-related warnings from module 'NonStrictModule'}}

actor A {
  func f() -> [StrictStruct: NonStrictClass] { [:] }
}

class NS { } // expected-note {{class 'NS' does not conform to the 'Sendable' protocol}}
// expected-targeted-complete-note@-1 {{class 'NS' does not conform to the 'Sendable' protocol}}

struct MyType { // expected-complete-note {{consider making struct 'MyType' conform to the 'Sendable' protocol}}
  var nsc: NonStrictClass
}

struct MyType2: Sendable {
  var nsc: NonStrictClass // expected-warning{{stored property 'nsc' of 'Sendable'-conforming struct 'MyType2' has non-sendable type 'NonStrictClass'}}
  var ns: NS // expected-warning{{stored property 'ns' of 'Sendable'-conforming struct 'MyType2' has non-sendable type 'NS'}}
}

func testA(ns: NS, mt: MyType, mt2: MyType2, sc: StrictClass, nsc: NonStrictClass) async {
  Task {
    print(ns) // expected-targeted-complete-warning{{capture of 'ns' with non-sendable type 'NS' in a `@Sendable` closure}}
    print(mt) // no warning with targeted: MyType is Sendable because we suppressed NonStrictClass's warning
    // expected-complete-warning @-1 {{capture of 'mt' with non-sendable type 'MyType' in a `@Sendable` closure}}
    print(mt2)
    print(sc) // expected-warning {{capture of 'sc' with non-sendable type 'StrictClass' in a `@Sendable` closure}}
    print(nsc) // expected-targeted-complete-warning {{capture of 'nsc' with non-sendable type 'NonStrictClass' in a `@Sendable` closure}}
  }
}

extension NonStrictStruct: @unchecked @retroactive Sendable { }

class StrictSubclass: StrictClass {
  override func send(_ body: () -> ()) {}
  override func dontSend(_ body: @Sendable () -> ()) {} // expected-warning {{declaration 'dontSend' has a type with different sendability from any potential overrides}}
}

struct StrictConformer: StrictProtocol {
  func send(_ body: () -> Void) {}
  func dontSend(_ body: @Sendable () -> Void) {} // expected-warning {{sendability of function types in instance method 'dontSend' does not match requirement in protocol 'StrictProtocol'}}
}

class NonStrictSubclass: NonStrictClass {
  override func send(_ body: () -> ()) {}
  override func dontSend(_ body: @Sendable () -> ()) {} // expected-warning {{declaration 'dontSend' has a type with different sendability from any potential overrides}}
}

struct NonStrictConformer: NonStrictProtocol {
  func send(_ body: () -> Void) {}
  func dontSend(_ body: @Sendable () -> Void) {} // expected-warning {{sendability of function types in instance method 'dontSend' does not match requirement in protocol 'NonStrictProtocol'}}
}
