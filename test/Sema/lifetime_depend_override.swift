// RUN: %target-typecheck-verify-swift -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

// rdar://192737567: class overrides must have the same lifetime dependencies
// as the overridden declaration.

struct NE: ~Escapable {
  @_lifetime(immortal) init() {}
}

class Base {
  @_lifetime(borrow ne) func borrowing(_ ne: NE) -> NE { ne } // expected-note {{overridden declaration is here}}
  @_lifetime(copy ne) func copying(_ ne: NE) -> NE { ne } // expected-note {{overridden declaration is here}}
  @_lifetime(copy ne) func matching(_ ne: NE) -> NE { ne }
  @_lifetime(copy a) func twoParams(_ a: NE, _ b: NE) -> NE { a } // expected-note {{overridden declaration is here}}
  @_lifetime(immortal) func immortal(_ ne: NE) -> NE { NE() } // expected-note {{overridden declaration is here}}
  @_lifetime(copy ne) class func classMethod(_ ne: NE) -> NE { ne } // expected-note {{potential overridden class method 'classMethod' here}}
  @_lifetime(a: copy b) func inoutTarget(_ a: inout NE, _ b: NE) { a = b } // expected-note {{overridden declaration is here}}
  @_lifetime(a: copy b) func inoutMatching(_ a: inout NE, _ b: NE) { a = b }
  func inferred(_ ne: NE) -> NE { ne }

  // Escapable result: dependencies are ignored.
  func escapable(_ ne: NE) -> Int { 0 }

  subscript(ne: NE) -> NE {
    @_lifetime(copy ne) get { ne } // expected-note {{overridden declaration is here}}
  }
}

class Derived: Base {
  @_lifetime(copy ne) override func borrowing(_ ne: NE) -> NE { ne } // expected-error {{instance method 'borrowing' has a lifetime dependence that does not match the overridden declaration}}
  @_lifetime(borrow ne) override func copying(_ ne: NE) -> NE { ne } // expected-error {{instance method 'copying' has a lifetime dependence that does not match the overridden declaration}}
  @_lifetime(copy ne) override func matching(_ ne: NE) -> NE { ne }
  @_lifetime(copy a, copy b) override func twoParams(_ a: NE, _ b: NE) -> NE { a } // expected-error {{instance method 'twoParams' has a lifetime dependence that does not match the overridden declaration}}
  @_lifetime(copy ne) override func immortal(_ ne: NE) -> NE { ne } // expected-error {{instance method 'immortal' has a lifetime dependence that does not match the overridden declaration}}
  // Lifetime dependencies of static methods are part of the compared type.
  @_lifetime(borrow ne) override class func classMethod(_ ne: NE) -> NE { ne } // expected-error {{method does not override any method from its superclass}}
  @_lifetime(a: borrow b) override func inoutTarget(_ a: inout NE, _ b: NE) {} // expected-error {{instance method 'inoutTarget' has a lifetime dependence that does not match the overridden declaration}}
  @_lifetime(a: copy b) override func inoutMatching(_ a: inout NE, _ b: NE) { a = b }
  @_lifetime(copy ne) override func inferred(_ ne: NE) -> NE { ne }
  override func escapable(_ ne: NE) -> Int { 0 }

  override subscript(ne: NE) -> NE {
    @_lifetime(borrow ne) get { ne } // expected-error {{getter for subscript 'subscript(_:)' has a lifetime dependence that does not match the overridden declaration}}
  }
}
