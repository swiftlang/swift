// {"kind":"typecheck","signature":"swift::constraints::Solution::computeSubstitutions(swift::NullablePtr<swift::ValueDecl>, swift::GenericSignature, swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (rootParameterPacks.size() >= 1), function getSingletonPackExpansion"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a func b < each c : a {
  b
