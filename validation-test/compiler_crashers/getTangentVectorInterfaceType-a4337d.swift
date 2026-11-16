// {"kind":"typecheck","original":"b208a1bd","signature":"getTangentVectorInterfaceType(swift::Type, swift::DeclContext*)","signatureAssert":"Assertion failed: (conf && \"Contextual type must conform to `Differentiable`\"), function getTangentVectorInterfaceType","signatureNext":"DerivedConformance::deriveDifferentiable"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
protocol a {
  associatedtype b
}
struct c<d: a
  extension c: Differentiable where d.b ==
