// {"kind":"typecheck","original":"73696db0","signature":"swift::GenericSignatureImpl::isReducedType(swift::Type) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a
struct b<c
  extension b: BidirectionalCollection where c: a
    protocol a: BidirectionalCollection where SubSequence == b<Self> {
d
