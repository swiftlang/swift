// {"kind":"typecheck","original":"2515e28c","signature":"swift::ASTWalker::PostWalkResult<swift::AssignExpr*> (anonymous namespace)::Verifier::dispatchVisitPost<swift::AssignExpr*>(swift::AssignExpr*)","signatureNext":"Verifier::walkToExprPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b> {
  static subscript(_enclosingInstance c: d, wrapped e: KeyPath<d, b>, storage f: KeyPath<d, a>) -> b
  {
  }
  var wrappedValue: b {
    get {
    }
    set {
    }
  }
}
class d {
  @a var h: g
}
protocol g {
}
