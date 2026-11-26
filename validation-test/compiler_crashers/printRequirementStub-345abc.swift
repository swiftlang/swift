// {"frontendArgs":["-typecheck"],"kind":"custom","signature":"swift::printRequirementStub(swift::ValueDecl*, swift::DeclContext*, swift::Type, swift::SourceLoc, llvm::raw_ostream&, bool)","signatureAssert":"Assertion failed: (T->mayHaveMembers()), function TypeTransformContext","signatureNext":"MultiConformanceChecker::checkAllConformances"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@reparentable
protocol P {
  func f()
}
protocol Q {
}
extension Q: @reparented P {
}
