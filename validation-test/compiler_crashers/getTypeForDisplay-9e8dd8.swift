// {"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"63ca9a72","signature":"getTypeForDisplay(swift::ValueDecl*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"getRequirementTypeForDisplay"}
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 %s
struct a {
  protocol b {
    init(c: () -> some d
    init(c: () -> some d
  }
  struct e: b
    let f = e
