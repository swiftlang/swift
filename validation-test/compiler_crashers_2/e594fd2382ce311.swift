// {"kind":"typecheck","signature":"swift::constraints::MissingCallFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < each b {
  init(repeat each b) func c < each d { a<repeat each d, String >(repeat e, {
    ""
