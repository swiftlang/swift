// {"kind":"typecheck","original":"3bd8e072","signature":"(anonymous namespace)::ConnectedComponents::unionSets(swift::TypeVariableType*, swift::TypeVariableType*)","signatureAssert":"Assertion failed: (validComponentCount > 0), function unionSets","signatureNext":"ConstraintGraph::computeConnectedComponents"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
let a = (0, 5 {
  b
  a
