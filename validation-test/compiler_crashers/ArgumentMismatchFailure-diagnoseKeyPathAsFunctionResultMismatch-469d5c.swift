// {"kind":"typecheck","original":"4a332ff6","signature":"swift::constraints::ArgumentMismatchFailure::diagnoseKeyPathAsFunctionResultMismatch() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ArgumentMismatchFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(repeat each b) where repeat each b: AnyKeyPath
a(\c
d
, .e
