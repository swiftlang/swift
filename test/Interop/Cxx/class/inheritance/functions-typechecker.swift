// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}functions.h

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

func callfreeFuncRenamedToInit() -> Int32 {
  var result: Int32 = 0
  result += freeFuncRenamedToInit()
  result += `init`() // expected-error {{cannot find 'init' in scope}}
  return result
}
