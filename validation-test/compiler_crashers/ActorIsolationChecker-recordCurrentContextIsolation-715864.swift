// {"kind":"typecheck","original":"70446496","signature":"(anonymous namespace)::ActorIsolationChecker::recordCurrentContextIsolation(swift::CurrentContextIsolationExpr*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ActorIsolationChecker::checkDefaultArgument"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b {
  wrappedValue: b  init(
    wrappedValue: b() =
      #isolation
}
actor c {
  @a  d = ""
