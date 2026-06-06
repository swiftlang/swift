// RUN: %target-typecheck-verify-swift

// Without the AssociatedTypeDisambiguation feature, the protocol-qualified
// type alias syntax is rejected at parse time.

protocol Alpha { associatedtype Item }

struct S {
  typealias Alpha.Item = Int // expected-error {{expected '=' in type alias declaration}}
}
