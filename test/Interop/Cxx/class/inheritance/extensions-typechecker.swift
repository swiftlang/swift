// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-experimental-cxx-interop

import Functions

extension Base {
  public func swiftFunc() {}
}

Derived().swiftFunc() // expected-error {{value of type 'Derived' has no member 'swiftFunc'}}
