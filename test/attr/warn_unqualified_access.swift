// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module-path %t/Other1.swiftmodule -module-name Other1 %S/Inputs/warn_unqualified_access_other.swift
// RUN: %target-swift-frontend -emit-module-path %t/Other2.swiftmodule -module-name Other2 %S/Inputs/warn_unqualified_access_other.swift
// RUN: %target-swift-frontend -I %t -typecheck %s -verify

import Other1
import Other2

@warn_unqualified_access // expected-error {{@warn_unqualified_access may only be used on 'func' declarations}} {{1-26=}}
var x: Int { return 0 }

@warn_unqualified_access // expected-error {{@warn_unqualified_access may only be used on 'func' declarations}} {{1-26=}}
struct X {}

@warn_unqualified_access // expected-error {{only methods can be declared @warn_unqualified_access}} {{1-26=}}
func topLevel() {
}

class Base {
  @warn_unqualified_access
  func a() {} // expected-note * {{declared here}}

  @warn_unqualified_access
  func toBeOverridden() {} // no-warning
}

extension Base {
  @warn_unqualified_access
  func b() {} // expected-note * {{declared here}}
}

class Sub : Base {
  @warn_unqualified_access
  func c() {} // expected-note * {{declared here}}

  override func toBeOverridden() {}

  func test() {
    a() // expected-warning {{use of 'a' treated as a reference to instance method in class 'Base'}} expected-note{{use 'self.' to silence this warning}} {{5-5=self.}}
    self.a()
    b() // expected-warning {{use of 'b' treated as a reference to instance method in class 'Base'}} expected-note{{use 'self.' to silence this warning}} {{5-5=self.}}
    self.b()
    c() // expected-warning {{use of 'c' treated as a reference to instance method in class 'Sub'}} expected-note{{use 'self.' to silence this warning}} {{5-5=self.}}
    self.c()

    toBeOverridden() // no-warning
  }

  func testWithoutCalling() {
    _ = a // expected-warning {{use of 'a' treated as a reference to instance method in class 'Base'}} expected-note{{use 'self.' to silence this warning}} {{9-9=self.}}
    _ = b // expected-warning {{use of 'b' treated as a reference to instance method in class 'Base'}} expected-note{{use 'self.' to silence this warning}} {{9-9=self.}}
    _ = c // expected-warning {{use of 'c' treated as a reference to instance method in class 'Sub'}} expected-note{{use 'self.' to silence this warning}} {{9-9=self.}}
  }
}

func test(_ sub: Sub) {
  sub.a()
  sub.b()
  sub.c()

  @warn_unqualified_access // expected-error {{only methods can be declared @warn_unqualified_access}} {{3-28=}}
  func inner() {
  }
  inner()
}

class Recovery {
  @warn_unqualified_access
  func topLevel() {} // expected-note * {{declared here}}
  @warn_unqualified_access
  func overloaded(_ x: Float) {} // expected-note * {{declared here}}

  func test() {
    topLevel() // expected-warning {{use of 'topLevel' treated as a reference to instance method in class 'Recovery'}}
    // expected-note@-1 {{use 'self.' to silence this warning}} {{5-5=self.}}
    // expected-note@-2 {{use 'warn_unqualified_access.' to reference the global function}} {{5-5=warn_unqualified_access.}}

    overloaded(5) // expected-warning {{use of 'overloaded' treated as a reference to instance method in class 'Recovery'}}
    // expected-note@-1 {{use 'self.' to silence this warning}} {{5-5=self.}}
    // expected-note@-2 {{use 'Other1.' to reference the global function in module 'Other1'}} {{5-5=Other1.}}
    // expected-note@-3 {{use 'Other2.' to reference the global function in module 'Other2'}} {{5-5=Other2.}}
  }
}
