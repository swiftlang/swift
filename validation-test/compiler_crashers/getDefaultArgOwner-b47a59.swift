// {"kind":"typecheck","original":"279925f0","signature":"getDefaultArgOwner(swift::ConcreteDeclRef, unsigned int)","signatureNext":"ExprRewriter::coerceCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b {
    c<each d>(e: repeat (repeat each b) -> each d)
  {
    repeat (each e)(
