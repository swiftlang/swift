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
extension Struct.TangentVector: Protocol {}
