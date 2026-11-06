// {"kind":"typecheck","original":"3a87b80a","signature":"(anonymous namespace)::ConstraintGenerator::visitApplyExpr(swift::ApplyExpr*)","signatureNext":"ConstraintWalker::walkToExprPost"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  struct c: a {
      d<e: a>(e        .Type) -> e.b.Type
    {
      d(c)(
