// {"kind":"typecheck","signature":"swift::constraints::AllowTypeOrInstanceMemberFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (TypeDC->isTypeContext() && \"Expected type decl context!\"), function diagnoseAsError","signatureNext":"AllowTypeOrInstanceMember::diagnose"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a func b<c >(c = a [
       extension a {
subscript->Int
