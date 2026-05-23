// {"kind":"typecheck","signature":"swift::ObjCKeyPathStringRequest::evaluate(swift::Evaluator&, swift::KeyPathExpr*, swift::DeclContext*) const","signatureAssert":"Assertion failed: (!empty()), function front","signatureNext":"ObjCKeyPathStringRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
#keyPath(
print
