// {"kind":"typecheck","original":"9a1e5852","signature":"swift::constraints::Solution::getFixedType(swift::TypeVariableType*) const","signatureAssert":"Assertion failed: (knownBinding != typeBindings.end()), function getFixedType","signatureNext":"diagnoseContextualFunctionCallGenericAmbiguity"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>( () -> b) -> b
func a<b: Numeric>( () -> b) -> b
let : Int = a {
