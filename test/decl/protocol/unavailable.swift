// RUN: %target-typecheck-verify-swift

protocol P { }

struct X { }

@available(*, unavailable)
extension X: P { }

struct Y<T: P> { }

@available(*, unavailable)
extension Y {
  // Okay, because the unavailable conformance is used within an
  // unavailable context.
  init() where T == X { }
}
