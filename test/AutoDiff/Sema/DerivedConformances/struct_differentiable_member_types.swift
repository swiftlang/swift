// RUN: %target-typecheck-verify-swift

import _Differentiation

// Test usages of synthesized `Differentiable` member struct types.

// TF-466: Test conforming a synthesized member type to a protocol with
// property requirements.
protocol Protocol {
  var weight: Float { get }
}
struct Struct: Differentiable {
  var weight: Float
}

// Extending a synthesized type doesn't actually work because of circularity issues.
// It just wasn't diagnosed before.

extension Struct.TangentVector: Protocol {}
// expected-error@-1 {{non-nominal type 'Struct.TangentVector' cannot be extended}}
