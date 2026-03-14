// {"kind":"typecheck","signature":"swift::GenericSignatureImpl::getSuperclassBound(swift::Type) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  typealias b = c class e : a protocol d : e, d.b
