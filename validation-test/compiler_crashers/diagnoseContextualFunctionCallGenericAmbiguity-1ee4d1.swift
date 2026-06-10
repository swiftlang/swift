// {"kind":"typecheck","original":"315bd2bd","signature":"diagnoseContextualFunctionCallGenericAmbiguity(swift::constraints::ConstraintSystem&, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>, llvm::ArrayRef<std::__1::pair<swift::constraints::Solution const*, swift::constraints::ConstraintFix const*>>)","signatureAssert":"Assertion failed: (idx < size()), function operator[]","signatureNext":"ConstraintSystem::diagnoseAmbiguityWithFixes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
struct c<e>: a
  func g<each h: a, i>(repeat each h,  (repeat each h.b) -> i) -> i
  let j = c<Float>(
  let k = c<Double>()
  let : Double = g(j    k) {
    (f:Float
    d
    : Double
  }
  ) -> Double
