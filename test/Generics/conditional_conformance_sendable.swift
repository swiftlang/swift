// RUN: %target-typecheck-verify-swift

// Conditional conformance edge case. If a type conforms to Sendable
// conditionally, allow some of those conditional requirements to
// reference unavailable Sendable conformances when allowMissing=true.

class ConditionalC {}

@available(*, unavailable)
extension ConditionalC : Sendable {}

struct ConditionalG<T> {}

extension ConditionalG : Sendable where T : Sendable {}

// Strict concurrency checking should reject this since the inherited
// conformance to Sendable has an unsatisfied conditional requirement.
protocol ConditionalDerived : Sendable {}

extension ConditionalG : ConditionalDerived {}

protocol ConditionalP {
  associatedtype Value: ConditionalDerived
}

// Value is concretized via a conditional conformance of
// ConditionalG<ConditionalC> to Sendable.
//
// The conditional requirement ConditionalC: Sendable is satisfied
// by an unavailable conformance. But we still allow it in this case.
extension ConditionalP where Value == ConditionalG<ConditionalC> {}
