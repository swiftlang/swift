// {"kind":"typecheck","signature":"swift::LifetimeDependenceChecker::diagnoseMissingResultDependencies(swift::DiagID)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@abi(func a->b) func a < b
