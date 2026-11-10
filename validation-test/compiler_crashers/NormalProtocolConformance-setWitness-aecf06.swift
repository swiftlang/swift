// {"kind":"typecheck","original":"8ed04fce","signature":"swift::NormalProtocolConformance::setWitness(swift::ValueDecl*, swift::Witness) const","signatureAssert":"Assertion failed: ((!isComplete() || isInvalid() || (dyn_cast<FuncDecl>(requirement) ? (dyn_cast<FuncDecl>(requirement)->isDistributed() || dyn_cast<FuncDecl>(requirement)->isDistributedThunk()) : false) || requirement->getAttrs().hasAttribute<OptionalAttr>() || requirement->isUnavailable()) && \"Conformance already complete?\"), function setWitness","signatureNext":"ConformanceChecker::recordWitness"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
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
