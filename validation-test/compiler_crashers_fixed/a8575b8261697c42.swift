// {"kind":"typecheck","original":"9e6249db","signature":"(anonymous namespace)::ConstraintGenerator::visitApplyExpr(swift::ApplyExpr*)"}
// RUN: not %target-swift-frontend -typecheck %s
(a as Sequence).Element {
}
