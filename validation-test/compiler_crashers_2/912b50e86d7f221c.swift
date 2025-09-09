// {"kind":"typecheck","signature":"swift::constraints::AllowTypeOrInstanceMemberFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (TypeDC->isTypeContext() && \"Expected type decl context!\"), function diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a func b<c >(c = a [
       extension a {
subscript->Int
