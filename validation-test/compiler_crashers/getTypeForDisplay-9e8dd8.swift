// {"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"63ca9a72","signature":"getTypeForDisplay(swift::ValueDecl*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"getRequirementTypeForDisplay"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 %s
struct a {
  protocol b {
    init(c: () -> some d
    init(c: () -> some d
  }
  struct e: b
    let f = e
