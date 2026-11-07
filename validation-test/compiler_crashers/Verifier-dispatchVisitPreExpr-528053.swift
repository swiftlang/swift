// {"kind":"typecheck","original":"269514b3","signature":"swift::ASTWalker::PreWalkResult<swift::Expr*> (anonymous namespace)::Verifier::dispatchVisitPreExpr<swift::OpenExistentialExpr*>(swift::OpenExistentialExpr*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"Traversal::doIt"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  protocol a
    struct b {
      subscript<c: a>(c )  c
    }
    fallthrough
    var d: b
    let e: a  d[e
