// {"kind":"typecheck","original":"8ed04fce","signature":"swift::NormalProtocolConformance::setWitness(swift::ValueDecl*, swift::Witness) const","signatureAssert":"Assertion failed: ((!isComplete() || isInvalid() || (dyn_cast<FuncDecl>(requirement) ? (dyn_cast<FuncDecl>(requirement)->isDistributed() || dyn_cast<FuncDecl>(requirement)->isDistributedThunk()) : false) || requirement->getAttrs().hasAttribute<OptionalAttr>() || requirement->isUnavailable()) && \"Conformance already complete?\"), function setWitness","signatureNext":"ConformanceChecker::recordWitness"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol c: a {
  d->[b]
}
extension c {
  d->[b]
  struct e: c {
    func == (f: e) {
      f.d
    }
  }
}
