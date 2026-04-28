// {"kind":"typecheck","original":"3907d1a2","signature":"swift::rewriting::RewriteContext::getRequirementMachine(swift::CanGenericSignature)","signatureNext":"GenericSignatureImpl::lookupNestedType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c: d where c.e.f == g
}
protocol
  h
{
  associatedtype f: a where f.b == Self
}
protocol d {
  associatedtype e: h
}
struct g: a {
}
