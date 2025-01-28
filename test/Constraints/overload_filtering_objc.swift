// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

// REQUIRES: objc_interop

// This test ensures that we are filtering out overloads based on argument
// labels, arity, etc., before those terms are visited. 

import Foundation

@objc protocol P {
  func foo(_ i: Int) -> Int // expected-note {{'foo' previously declared here}}
  func foo(_ d: Double) -> Int // expected-warning {{method 'foo' with Objective-C selector 'foo:' conflicts with previous declaration with the same Objective-C selector; this is an error in the Swift 6 language mode}}

  @objc optional func opt(_ i: Int) -> Int
  @objc optional func opt(double: Double) -> Int
  
  subscript(i: Int) -> String { get }
}

func testOptional(obj: P) {
  // CHECK: [disabled] $T2 bound to decl overload_filtering_objc.(file).P.opt(double:)
  _ = obj.opt?(1)
}

