// {"kind":"typecheck","original":"ff06ed53","signature":"(anonymous namespace)::ResolvePattern::visitTupleExpr(swift::TupleExpr*)","signatureAssert":"Assertion failed: (!elts[0].getLabel().empty()), function create","signatureNext":"ResolvePattern::visitBindOptionalExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch <#expression#> {
case (repeat a)?:
}
