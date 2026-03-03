// {"kind":"typecheck","original":"470b69c1","signature":"swift::constraints::ConstraintSystem::matchFunctionResultTypes(swift::Type, swift::Type, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (isDecl() && \"only makes sense for declaration choices\"), function getFunctionRefInfo"}
// RUN: not --crash %target-swift-frontend -typecheck %s
(a:-).a {
}(<#expression#>)
