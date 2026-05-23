// {"kind":"typecheck","original":"4bf23d59","signature":"(anonymous namespace)::ResolvePattern::visitTupleExpr(swift::TupleExpr*)","signatureAssert":"Assertion failed: (!elts[0].getLabel().empty()), function create","signatureNext":"ResolvePattern::visitSequenceExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
for case (repeat a) as b in <#expression#> {
}
