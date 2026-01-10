// {"kind":"complete","original":"26141d06","signature":"swift::NormalProtocolConformance::setWitness(swift::ValueDecl*, swift::Witness) const","signatureAssert":"Assertion failed: ((!isComplete() || isInvalid() || (dyn_cast<FuncDecl>(requirement) ? (dyn_cast<FuncDecl>(requirement)->isDistributed() || dyn_cast<FuncDecl>(requirement)->isDistributedThunk()) : false) || requirement->getAttrs().hasAttribute<OptionalAttr>() || requirement->isUnavailable()) && \"Conformance already complete?\"), function setWitness"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a: OptionSet {
  #^^#
}
