// {"kind":"typecheck","signature":"swift::LifetimeDependenceChecker::LifetimeDependenceChecker(swift::AbstractFunctionDecl*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@abi(func a->b) func a < b
