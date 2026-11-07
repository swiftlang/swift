// {"kind":"typecheck","original":"7048f9d7","signature":"swift::KeyPathExpr::getKeyPathType() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ExprAvailabilityWalker::walkToExprPre"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  convenience #a(\array)
}
