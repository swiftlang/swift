// {"kind":"typecheck","original":"1e8c9260","signature":"swift::constraints::Solution::getOverloadChoice(swift::constraints::ConstraintLocator*) const","signatureNext":"ExprRewriter::visitKeyPathExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
callAsFunction {
\a()
