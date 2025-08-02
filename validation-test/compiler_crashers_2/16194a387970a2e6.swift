// {"kind":"typecheck","signature":"(anonymous namespace)::ResolvePattern::visitTupleExpr(swift::TupleExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch { case (repeat a
