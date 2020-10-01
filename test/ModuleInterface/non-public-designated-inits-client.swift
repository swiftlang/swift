// Compile the imported module to a .swiftinterface and ensure the convenience
// init cannot be called.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %S/non-public-designated-inits.swift -emit-module-interface-path %t/Module.swiftinterface -module-name Module -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify %s -I %t

// Make sure the same error is emitted when importing a .swiftmodule

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Module.swiftmodule %S/non-public-designated-inits.swift -module-name Module -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify %s -I %t

import Module

open class B : A {
  var x: Int

  public override init(_ x: Int) {
    self.x = x
    super.init(x)
  }
}

print(B(hi: ())) // expected-error {{cannot convert value of type '()' to expected argument type 'Int'}}
// expected-error @-1 {{extraneous argument label 'hi:' in call}}
