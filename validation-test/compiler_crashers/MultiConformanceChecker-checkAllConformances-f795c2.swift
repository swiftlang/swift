// {"kind":"typecheck","original":"c950cc2e","signature":"(anonymous namespace)::MultiConformanceChecker::checkAllConformances()","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"TypeChecker::checkConformancesInContext"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b
  protocol c {
    associatedtype d
    typealias d = a
    struct e: c
