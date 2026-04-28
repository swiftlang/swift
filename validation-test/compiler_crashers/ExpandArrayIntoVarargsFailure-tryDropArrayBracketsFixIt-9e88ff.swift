// {"kind":"typecheck","original":"5f1a9536","signature":"swift::constraints::ExpandArrayIntoVarargsFailure::tryDropArrayBracketsFixIt(swift::Expr const*) const","signatureAssert":"Assertion failed: (!empty()), function back","signatureNext":"ExpandArrayIntoVarargsFailure::diagnoseAsNote"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct b {
  c->
    d<b>
  {
    .init([]
  }
  struct d<e {
    init(a: [e]
    init(_: e...
