// {"kind":"typecheck","original":"1e8c9260","signature":"swift::constraints::Solution::getOverloadChoice(swift::constraints::ConstraintLocator*) const","signatureNext":"ExprRewriter::visitKeyPathExpr"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
callAsFunction {
\a()
