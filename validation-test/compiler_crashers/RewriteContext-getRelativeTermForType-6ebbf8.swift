// {"kind":"typecheck","original":"a496ed11","signature":"swift::rewriting::RewriteContext::getRelativeTermForType(swift::CanType, llvm::ArrayRef<swift::rewriting::Term>)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"PropertyMap::computeConstraintTermForTypeWitness"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol c {
  associatedtype d : a
}
func i < e >() {
  struct f : a {
    typealias b = e
    func g < h where h : c, h.d == f>()
  }
}
