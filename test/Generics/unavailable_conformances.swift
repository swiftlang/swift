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

// A more elaborate setup that hits the conformance check in property map
// construction rather than concrete contraction.

protocol AssocP {}

@available(*, unavailable)
struct ConcreteP {}

@available(*, unavailable)
extension ConcreteP: AssocP {}

protocol Base {
  associatedtype T : AssocP
}

@available(*, unavailable)
extension Base where T == ConcreteP {}
