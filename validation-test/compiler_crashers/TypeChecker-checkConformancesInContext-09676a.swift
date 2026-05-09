// {"kind":"typecheck","original":"94caa3ba","signature":"swift::TypeChecker::checkConformancesInContext(swift::IterableDeclContext*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"DeclChecker::visitClassDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(extension conformances : Sendable) macro a()
@a class b {
  class c : b {
  }
}
