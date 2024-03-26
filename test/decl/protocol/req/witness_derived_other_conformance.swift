// RUN: %target-typecheck-verify-swift

protocol P {
  func hash(into: inout Hasher)
}

// P.hash(into:) is witnessed by the S.hash(into:) that is synthesized
// for the Hashable conformance.
struct S: P, Hashable {}

struct G<T> {}

extension G: Equatable where T: Equatable {}
extension G: Hashable where T: Hashable {}

// Same here, but the conformance is conditional.
extension G: P where T: P & Hashable {}
