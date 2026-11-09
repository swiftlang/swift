// {"kind":"typecheck","original":"cc00e867","signature":"swift::rewriting::RewriteContext::getRelativeTermForType(swift::CanType, llvm::ArrayRef<swift::rewriting::Term>)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"TypeTransform::doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol
  c
{
  associatedtype d: a
}
protocol e: a {
  struct f<g> {
    h {
    struct i : e {
    typealias b = f func j < k where k : c, k.d == i
      }
    }
