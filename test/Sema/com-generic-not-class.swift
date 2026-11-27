// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-com-interop -I %t %s

import COM

// A generic parameter constrained to a COM interface is class-only by
// conformance rule, but it is not a native class reference.

@com(interface: "51000000-0000-0000-0000-000000000001")
protocol IItem {}

@com(interface: "51000000-0000-0000-0000-000000000002")
protocol IClassItem: IItem {}

func requiresClass<U: AnyObject>(_ u: U) {} // expected-note {{where 'U' = 'T'}}

func pass<T: IClassItem>(_ x: T, _ y: T) {
  requiresClass(x) // expected-error {{global function 'requiresClass' requires that 'T' be a class type}}
  _ = x === y // expected-error 2 {{argument type 'T' expected to be an instance of a class or class-constrained type}}
  _ = ObjectIdentifier(x) // expected-error {{argument type 'T' expected to be an instance of a class or class-constrained type}}
}
