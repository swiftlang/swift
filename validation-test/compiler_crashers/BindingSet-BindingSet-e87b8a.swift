// {"kind":"typecheck","original":"d0f78d89","signature":"swift::constraints::inference::BindingSet::BindingSet(swift::constraints::ConstraintSystem&, swift::TypeVariableType*, swift::constraints::inference::PotentialBindings const&)","signatureAssert":"Assertion failed: (elementTy->mayEscape()), function applyElementTypeToBinding","signatureNext":"ConstraintGraphNode::initBindingSet"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(c: [b]) {
  c.withUnsafeBufferPointer { d in
    UnsafeRawPointer + d
  }
}
