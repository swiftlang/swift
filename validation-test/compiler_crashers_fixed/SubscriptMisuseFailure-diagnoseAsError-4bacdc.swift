// {"kind":"typecheck","original":"cb606608","signature":"swift::constraints::SubscriptMisuseFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not %target-swift-frontend -typecheck %s
a(.subscript < b)
