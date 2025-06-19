// {"signature":"swift::diagnoseArgumentLabelError(swift::ASTContext&, swift::ArgumentList const*, llvm::ArrayRef<swift::Identifier>, swift::ParameterContext, swift::InFlightDiagnostic*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>((), c : ()->b) a {}
c : {
