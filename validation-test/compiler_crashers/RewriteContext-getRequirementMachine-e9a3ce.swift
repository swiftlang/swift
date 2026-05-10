// {"kind":"typecheck","original":"f5cd8d1f","signature":"swift::rewriting::RewriteContext::getRequirementMachine(swift::CanGenericSignature)","signatureNext":"GenericSignatureImpl::getReducedType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: a
  associatedtype c: a where c.b == b.c
}
struct d<e: a>: a where e.b.c == d {
}
