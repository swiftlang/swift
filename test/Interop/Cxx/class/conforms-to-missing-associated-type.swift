// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/include)
// RUN: split-file %s %t
//
// RUN: %target-swift-frontend -typecheck -module-name a -cxx-interoperability-mode=default -I %t/include %t/a.swift -verify

//--- include/module.modulemap
module cxx {
  header "header.h"
  export *
}

//--- include/header.h
struct S {
  S() {}
} __attribute__((swift_attr("conforms_to:a.P")));

//--- a.swift
import cxx
// expected-error@+2{{type 'S' does not conform to protocol 'P'}}
// expected-note@+1{{protocol requires nested type 'A'}}
public protocol P {
  associatedtype A
  func foo(_: A)
}
extension P {
  func foo(_: A) {}
}
func test(x : S) {
    x.foo(0) // expected-error{{cannot convert value of type 'Int' to expected argument type 'S.A'}}
}
