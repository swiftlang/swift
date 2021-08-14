// RUN: %target-swift-frontend -typecheck -verify %s

import _Differentiation

protocol TangentVectorP: Differentiable {
  // expected-note @+1 {{protocol requires property 'requirement' with type 'Int'; do you want to add a stub?}}
  var requirement: Int { get }
}

protocol TangentVectorConstrained: Differentiable where TangentVector: TangentVectorP {}

struct StructWithTangentVectorConstrained: TangentVectorConstrained {
  var x: Float
}
// expected-error @-1 {{type 'StructWithTangentVectorConstrained.TangentVector' does not conform to protocol 'TangentVectorP'}}
