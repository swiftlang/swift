// {"kind":"typecheck","original":"96fa6e3e","signature":"swift::StorageImplInfoRequest::evaluate(swift::Evaluator&, swift::AbstractStorageDecl*) const","signatureAssert":"Assertion failed: (info.hasStorage() == storage->hasStorage() || storage->getASTContext().Diags.hadAnyError()), function evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  func b() {
    override var c: Any {
      didSet {
      }
    }
  }
}
