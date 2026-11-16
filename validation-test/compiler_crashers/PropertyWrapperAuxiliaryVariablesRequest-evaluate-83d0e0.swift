// {"kind":"typecheck","signature":"swift::PropertyWrapperAuxiliaryVariablesRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const","signatureNext":"PropertyWrapperAuxiliaryVariablesRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  var wrappedValue: b
  var projectedValue {
    @a @_projectedValueProperty(c) var d
  }
}
