// {"kind":"typecheck","signature":"(anonymous namespace)::ConnectedComponents::unionSets(swift::TypeVariableType*, swift::TypeVariableType*)","signatureAssert":"Assertion failed: (validComponentCount > 0), function unionSets","signatureNext":"ConstraintGraph::computeConnectedComponents"}
// RUN: not --crash %target-swift-frontend -typecheck %s
let b = (c(), d()
a {
  e
  b
