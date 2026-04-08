// {"kind":"typecheck","original":"35eb43bc","signature":"swift::constraints::KeyPathSubscriptIndexHashableFailure::getLoc() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"KeyPathSubscriptIndexHashableFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  subscript( Hashable)  Int
  struct b {  c = \a[b]
