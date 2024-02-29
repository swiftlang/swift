// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/API.swift \
// RUN:   -module-name API -swift-version 5 \
// RUN:   -emit-module-path %t/API.swiftmodule

// Build client with module
// RUN: %target-swift-emit-silgen \
// RUN:   -I %t \
// RUN:   -module-name Client \
// RUN:    %t/src/Client.swift -verify

//--- API.swift

public class C {}

public protocol P1 {
  typealias A = (C & P1)
}

public protocol P2 {
  typealias F = (P2) -> Void
}

public protocol P3 where Self : C {
}

//--- Client.swift
import API

func test(_: any P1) {} // Ok

_ = P2.self // Ok

class B : P3 {
  // expected-error@-1 {{type 'B' does not conform to protocol 'P3'}}
  // expected-error@-2 {{'P3' requires that 'B' inherit from 'C'}}
  // expected-note@-3 {{requirement specified as 'Self' : 'C' [with Self = B]}}
}
