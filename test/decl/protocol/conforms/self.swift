// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype T = Int

  func hasDefault()
  func returnsSelf() -> Self
  func hasDefaultTakesT(_: T)
  func returnsSelfTakesT(_: T) -> Self
}

extension P {
  func hasDefault() {}

  func returnsSelf() -> Self {
    return self
  }

  func hasDefaultTakesT(_: T) {}

  func returnsSelfTakesT(_: T) -> Self { // expected-error {{method 'returnsSelfTakesT' in non-final class 'Class' cannot be implemented in a protocol extension because it returns `Self` and has associated type requirements}}
    return self
  }
}

// This fails
class Class : P {}

// This succeeds, because the class is final
final class FinalClass : P {}

// This succeeds, because we're not using the default implementation
class NonFinalClass : P {
  func returnsSelfTakesT(_: T) -> Self {
    return self
  }
}
