// {"extraArgs":["-language-mode","6"],"kind":"complete","original":"5adecb41","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"UnableToInferClosureParameterType::diagnoseAsError"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
@propertyWrapper struct a<b> {
  var wrappedValue: b
}
struct c {
  struct d {
    @e<
      #^^#
    > var i: () -> c
  }
  var f = d {
    @a({
      $0.g
    }) var h
  }
}
