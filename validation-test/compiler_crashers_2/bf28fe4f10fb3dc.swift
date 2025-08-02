// {"kind":"typecheck","signature":"(anonymous namespace)::ResolvePattern::visitCallExpr(swift::CallExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch { case .a(&b
