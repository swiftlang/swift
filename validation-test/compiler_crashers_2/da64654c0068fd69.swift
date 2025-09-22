// {"kind":"typecheck","original":"d896f132","signature":"swift::constraints::MissingConformanceFailure::MissingConformanceFailure(swift::constraints::Solution const&, swift::constraints::ConstraintLocator*, std::__1::pair<swift::Type, swift::Type>)","signatureAssert":"Assertion failed: (getGenericContext() && \"Affected decl not within a generic context?\"), function RequirementFailure"}
// RUN: not --crash %target-swift-frontend -typecheck %s
let a =
  {
    0 ? [a] : [1&&: <#expression#>]
  }[<#expression#>]
