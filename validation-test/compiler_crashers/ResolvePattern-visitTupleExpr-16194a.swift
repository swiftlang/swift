// {"kind":"typecheck","signature":"(anonymous namespace)::ResolvePattern::visitTupleExpr(swift::TupleExpr*)","signatureAssert":"Assertion failed: (!elts[0].getLabel().empty()), function create"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch { case (repeat a
