// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"865de0bf","signature":"swift::IsImplicitlyUnwrappedOptionalRequest::evaluate(swift::Evaluator&, swift::ValueDecl*) const","signatureAssert":"Assertion failed: (Index < this->size() && \"Invalid index!\"), function operator[]","signatureNext":"IsImplicitlyUnwrappedOptionalRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
{
  protocol a {
    associatedtype b
    var c: b
  }
  class d: a {
    var c: d {
      didSet
    }
  }
  e
}
