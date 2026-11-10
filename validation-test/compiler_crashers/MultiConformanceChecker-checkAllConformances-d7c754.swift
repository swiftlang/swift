// {"kind":"typecheck","original":"553ea614","signature":"(anonymous namespace)::MultiConformanceChecker::checkAllConformances()","signatureAssert":"Assertion failed: (getKind() == ActorInstance), function isActorInstanceForSelfParameter","signatureNext":"TypeChecker::checkConformancesInContext"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@globalActor actor a {
  static  shared =
  class b
    protocol c {
      f: b
      func g(b )
    }
    actor d: c {
      f: b
      @a func g(e: b
