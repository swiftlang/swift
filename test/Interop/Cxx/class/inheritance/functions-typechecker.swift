// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-experimental-cxx-interop

import Functions

extension Base {
  public func swiftFunc() {}
}

Derived().swiftFunc() // expected-error {{value of type 'Derived' has no member 'swiftFunc'}}

// ok, this calls the derived method.
Derived().sameMethodNameSameSignature()
Derived().sameMethodDifferentSignature(1)
// ok, this is the base class method.
Derived().sameMethodDifferentSignature()

// FIXME: we should import this (https://github.com/apple/swift/issues/69745):
Derived().rvalueThisInBase() // expected-error {{value of type 'Derived' has no member 'rvalueThisInBase'}}
