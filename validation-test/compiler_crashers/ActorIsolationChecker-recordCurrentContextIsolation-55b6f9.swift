// {"kind":"typecheck","original":"12ed7233","signature":"(anonymous namespace)::ActorIsolationChecker::recordCurrentContextIsolation(swift::CurrentContextIsolationExpr*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ActorIsolationChecker::walkToExprPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper
struct a<
  b
> {
  init(wrappedValue: b, actor: Actor)
  var wrappedValue: b
}
actor c {
  @a(
    actor:
      #isolation) var d = []
}
