// {"kind":"typecheck","original":"269514b3","signature":"swift::ASTWalker::PreWalkResult<swift::Expr*> (anonymous namespace)::Verifier::dispatchVisitPreExpr<swift::OpenExistentialExpr*>(swift::OpenExistentialExpr*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  protocol a
    struct b {
      subscript<c: a>(c )  c
    }
    fallthrough
    var d: b
    let e: a  d[e
