// {"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"74c5578c","signature":"swift::collectExistentialConformances(swift::CanType, swift::CanType, bool)","signatureAssert":"Assertion failed: (conformance), function collectExistentialConformances","signatureNext":"ExprRewriter::coerceExistential"}
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 %s
protocol a
  let b: a.Type
  func c<d>(d ) -> d
  _ = c(b
