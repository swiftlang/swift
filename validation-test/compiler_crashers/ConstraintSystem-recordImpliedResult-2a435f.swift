// {"kind":"typecheck","original":"b3234b81","signature":"swift::constraints::ConstraintSystem::recordImpliedResult(swift::Expr*, swift::constraints::ImpliedResultKind)","signatureAssert":"Assertion failed: (inserted && \"Duplicate implied result?\"), function recordImpliedResult"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[
  switch 0 {
  case .a(
    if <#expression#> {
      b
    }):
  }
]
