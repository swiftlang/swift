// {"kind":"typecheck","original":"279925f0","signature":"swift::getParameterAt(swift::ConcreteDeclRef, unsigned int)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"getDefaultArgOwner"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b {
    c<each d>(e: repeat (repeat each b) -> each d)
  {
    repeat (each e)(
