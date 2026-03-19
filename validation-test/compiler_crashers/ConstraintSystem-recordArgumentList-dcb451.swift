// {"kind":"typecheck","original":"b1c243c6","signature":"swift::constraints::ConstraintSystem::recordArgumentList(swift::constraints::ConstraintLocator*, swift::ArgumentList*)","signatureAssert":"Assertion failed: (inserted), function recordArgumentList","signatureNext":"ConstraintGenerator::addSubscriptConstraints"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  switch 0 {
  case .a(b[<#expression#>]):
  }
}
