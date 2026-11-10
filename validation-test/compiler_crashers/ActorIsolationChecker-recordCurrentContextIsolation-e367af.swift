// {"kind":"typecheck","original":"70446496","signature":"(anonymous namespace)::ActorIsolationChecker::recordCurrentContextIsolation(swift::CurrentContextIsolationExpr*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ActorIsolationChecker::getCurrentContextIsolation"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b {
  wrappedValue: b  init(wrappedValue: b
  isolated  Actor =
    #isolation
}
actor c {
  @a  d = ""
