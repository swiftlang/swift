// {"kind":"typecheck","original":"803bd5c6","signature":"swift::diagnoseArgumentLabelError(swift::ASTContext&, swift::ArgumentList const*, llvm::ArrayRef<swift::Identifier>, swift::ParameterContext, swift::InFlightDiagnostic*)","signatureAssert":"Assertion failed: ((numMissing + numExtra + numWrong > 0) && \"Should not call this function with nothing to diagnose\"), function diagnoseArgumentLabelError","signatureNext":"RelabelArguments::diagnoseForAmbiguity"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(
  b , b , _: b
  c: b -> Void = a(0, 0) {
    $ < 0
  } c: {
