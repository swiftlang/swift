// {"kind":"typecheck","original":"430aa3c7","signature":"checkSingleOverride(swift::ValueDecl*, swift::ValueDecl*)","signatureAssert":"Assertion failed: (ctx.Diags.hadAnyError()), function checkSingleOverride","signatureNext":"OverrideMatcher::checkPotentialOverrides"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c
  func d(during: c) async -> b
}
protocol e: a {
  associatedtype f: Error
  func d(during: c) async throws(f) -> b
}
