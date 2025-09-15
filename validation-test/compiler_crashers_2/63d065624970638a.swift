// {"signature":"swift::GenericSignatureImpl::lookupNestedType(swift::Type, swift::Identifier) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a.b
protocol c where a.d == Self
  protocol a {
    associatedtype e : c
    typealias b
    typealias d = Self.f
