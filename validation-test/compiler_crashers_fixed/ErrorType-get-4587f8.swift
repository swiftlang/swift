// {"kind":"typecheck","languageMode":6,"original":"00c07e95","signature":"swift::ErrorType::get(swift::Type)","signatureAssert":"Assertion failed: (!originalType->getRecursiveProperties().isSolverAllocated() && \"Solver-allocated error types not supported\"), function get"}
// RUN: not %target-swift-frontend -typecheck -swift-version 6 %s
protocol a<b, c> {
  associatedtype b
  associatedtype c
}
extension a {
  func
    compose<d>() -> a<d, d>
  {
    compose.compose
  }
}
