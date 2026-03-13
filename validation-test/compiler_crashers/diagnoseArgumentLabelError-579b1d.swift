// {"kind":"typecheck","signature":"swift::diagnoseArgumentLabelError(swift::ASTContext&, swift::ArgumentList const*, llvm::ArrayRef<swift::Identifier>, swift::ParameterContext, swift::InFlightDiagnostic*)","signatureAssert":"Assertion failed: ((numMissing + numExtra + numWrong > 0) && \"Should not call this function with nothing to diagnose\"), function diagnoseArgumentLabelError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>((), c : ()->b) a {}
c : {
