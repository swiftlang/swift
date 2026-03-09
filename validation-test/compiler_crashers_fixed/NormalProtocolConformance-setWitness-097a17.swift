// {"kind":"typecheck","original":"00ecd163","signature":"swift::NormalProtocolConformance::setWitness(swift::ValueDecl*, swift::Witness) const","signatureAssert":"Assertion failed: ((!isComplete() || isInvalid() || (dyn_cast<FuncDecl>(requirement) ? (dyn_cast<FuncDecl>(requirement)->isDistributed() || dyn_cast<FuncDecl>(requirement)->isDistributedThunk()) : false) || requirement->getAttrs().hasAttribute<OptionalAttr>() || requirement->isUnavailable()) && \"Conformance already complete?\"), function setWitness"}
// RUN: not %target-swift-frontend -typecheck %s
struct a: OptionSet
  let b  !a(
