// {"kind":"typecheck","original":"278d54b7","signature":"(anonymous namespace)::ExprRewriter::visitKeyPathExpr(swift::KeyPathExpr*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ExprWalker::walkToExprPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
callAsFunction
  var b : a
  let c = \b()
}
