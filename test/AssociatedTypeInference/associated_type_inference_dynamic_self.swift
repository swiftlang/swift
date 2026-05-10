// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A

  func f(_: A) -> Self
}

class C1: P {
  func f(_: Int) -> Self {}
}

class Base {
  func f(_: String) -> Self {}
}

class Derived: Base, P {}
