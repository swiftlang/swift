// RUN: not %target-swift-frontend %s -typecheck

public protocol I {
  associatedtype X : Equatable
}

protocol C : I {
}

protocol A : class {
  func u<T : Comparable>() -> T?
}

final class B<S : C> {
  func u(_ p: A) -> S.X {
    if let i = p.u() as S.X? {
      return i
    }
    fatalError("nope")
  }
}
