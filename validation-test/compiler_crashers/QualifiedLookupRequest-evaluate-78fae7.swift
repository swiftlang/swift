// {"kind":"typecheck","signature":"swift::QualifiedLookupRequest::evaluate(swift::Evaluator&, swift::DeclContext const*, llvm::SmallVector<swift::NominalTypeDecl*, 4u>, swift::DeclNameRef, swift::NLOptions) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"SimpleRequest::evaluateRequest"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  extension a {
  b {
    func c<d>(e : d) {
      e = f
