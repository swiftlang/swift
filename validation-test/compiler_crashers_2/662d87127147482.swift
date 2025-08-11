// {"kind":"typecheck","original":"2e2249a5","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<b> = (
>extension a where b ==  {
c
