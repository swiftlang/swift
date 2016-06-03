// RUN: %target-parse-verify-swift

protocol P {
  init()
}

class C : P { // expected-error {{initializer requirement 'init()' can only be satisfied by a `required` initializer in non-final class 'C'}}
  // No further errors.
}

class B : C {
  init(i: Int) {}
}

class D : B {}
