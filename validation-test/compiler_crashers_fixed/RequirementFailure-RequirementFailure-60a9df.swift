// {"kind":"typecheck","signature":"swift::constraints::RequirementFailure::RequirementFailure(swift::constraints::Solution const&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (getRequirementDC() && \"Couldn't find where the requirement came from?\"), function RequirementFailure"}
// RUN: %target-typecheck-verify-swift
extension Error where Self == <#type#> { // expected-error {{editor placeholder in source file}} expected-error {{cannot find type '<#type#>' in scope}}
  var a: Error {
    .a // expected-error {{instance member 'a' cannot be used on type 'any Error'}}
  }
}
