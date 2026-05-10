// {"kind":"typecheck","original":"0349ef3e","signature":"(anonymous namespace)::ConstraintGenerator::visitTupleExpr(swift::TupleExpr*)","signatureAssert":"Assertion failed: (!ty->is<InOutType>() && \"Cannot have InOutType in a tuple\"), function TupleTypeElt","signatureNext":"ConstraintWalker::walkToExprPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  b: {
    mutate {
      (c
      &d
