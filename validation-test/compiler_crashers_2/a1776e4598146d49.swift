// {"signature":"(anonymous namespace)::ResolvePattern::visitCallExpr(swift::CallExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b : _const a
