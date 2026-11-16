// {"kind":"typecheck","original":"a81cf1e7","signature":"swift::constraints::MissingContextualTypeForNil::diagnoseAsError()","signatureAssert":"Assertion failed: (isa<X>(Val) && \"cast_if_present<Ty>() argument of incompatible type!\"), function cast_if_present","signatureNext":"SpecifyContextualTypeForNil::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
if let
  a: _ = nil
{
}
