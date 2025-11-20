// {"kind":"typecheck","original":"5b785ef0","signature":"swift::ASTWalker::PreWalkResult<swift::Expr*> (anonymous namespace)::Verifier::dispatchVisitPreExpr<swift::OpenExistentialExpr*>(swift::OpenExistentialExpr*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension Dictionary {
    a(b: Sequence)
  {
    {
      for c  d      b {
        var values =
          [] ?? {
            values.append(c
