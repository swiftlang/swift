// RUN: %target-typecheck-verify-swift -solver-enable-diagnose-valid-salvage -swift-version 6

protocol P: Sendable {
  func f() -> G<Void>
}

struct G<T> {
  func method<U>(_: @escaping @Sendable (T) -> G<U>) -> G<U> {
    fatalError()
  }
  func method<U>(_: @escaping @Sendable (T) throws -> U) -> G<U> {
    fatalError()
  }
  func method<U>(_: G<U>) -> G<U> {
    fatalError()
  }
}

func f(p: any P) {
  // Disjunction pruning was disabling all three choices for method().
  // We would end up in salvage and diagnose an ambiguity.
  _ = p.f().method(p.f)
}
