// {"kind":"typecheck","signature":"swift::PropertyWrapperAuxiliaryVariablesRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  @a $b:
