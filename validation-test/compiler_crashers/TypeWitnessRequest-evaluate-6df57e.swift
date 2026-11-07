// {"kind":"typecheck","original":"4dc2216b","signature":"swift::TypeWitnessRequest::evaluate(swift::Evaluator&, swift::NormalProtocolConformance*, swift::AssociatedTypeDecl*) const","signatureAssert":"Assertion failed: (known != conformance->TypeWitnesses.end() && \"Didn't resolve witness?\"), function evaluate","signatureNext":"TypeWitnessRequest::OutputType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  struct c: a
    ;
    #d(c.b
