// {"kind":"typecheck","signature":"swift::constraints::SubscriptMisuseFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
"" [.subscript
