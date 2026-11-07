// {"kind":"typecheck","original":"0349ef3e","signature":"(anonymous namespace)::ConstraintGenerator::visitTupleExpr(swift::TupleExpr*)","signatureAssert":"Assertion failed: (!ty->is<InOutType>() && \"Cannot have InOutType in a tuple\"), function TupleTypeElt","signatureNext":"ConstraintWalker::walkToExprPost"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  b: {
    mutate {
      (c
      &d
