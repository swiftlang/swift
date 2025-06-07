// swift-interface-format-version: 1.0
// swift-compiler-version: Swift version 6.0
// swift-module-flags: -swift-version 5 -enable-library-evolution -module-name Test
// expected-error @-3 {{failed to verify module interface of 'Test'}}
import Swift
public struct S {
  @inlinable public func foo() {
    doesNotExist() // expected-error {{cannot find 'doesNotExist' in scope}}
  }
}

// RUN: %target-swift-typecheck-module-from-interface(%s) -module-name Test -verify -verify-ignore-unknown -experimental-lazy-typecheck
