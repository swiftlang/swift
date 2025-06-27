// {"signature":"swift::GenericSignatureImpl::prohibitsIsolatedConformance(swift::Type) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  typealias c = a
}
struct d : e
  protocol e : a {
    associatedtype c where b == c.b
    associatedtype f = Self
