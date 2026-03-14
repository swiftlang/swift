// {"kind":"typecheck","original":"1e8c9260","signature":"(anonymous namespace)::ExprRewriter::visitKeyPathExpr(swift::KeyPathExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
callAsFunction {
\a()
