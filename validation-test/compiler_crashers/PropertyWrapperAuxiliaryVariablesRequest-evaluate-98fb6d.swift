// {"kind":"typecheck","signature":"swift::PropertyWrapperAuxiliaryVariablesRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const","signatureNext":"PropertyWrapperAuxiliaryVariablesRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  @a $b:
