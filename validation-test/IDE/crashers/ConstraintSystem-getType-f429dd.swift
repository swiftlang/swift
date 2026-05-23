// {"kind":"complete","original":"9402aae6","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"UnableToInferClosureParameterType::diagnoseAsError"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper
struct a<b> {
  var wrappedValue: b
  init(c: <#type#>)  > @a( {
    @a({
      $0
      &&#^^#
    }) var d
  }
  = 0
  var e
}
