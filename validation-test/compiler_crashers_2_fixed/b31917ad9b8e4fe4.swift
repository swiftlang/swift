// {"kind":"typecheck","original":"0df5dc82","signature":"swift::constraints::ConstraintSystem::typeVarOccursInType(swift::TypeVariableType*, swift::Type, bool*)","signatureAssert":"Assertion failed: ((!typeVariables.empty() || hasError()) && \"Did not find type variables!\"), function getTypeVariables"}
// RUN: not %target-swift-frontend -typecheck %s
struct a<each b {
  typealias d<c> = (
  >  func  1 {
    typealias e = f
    e
    typealias e = d
