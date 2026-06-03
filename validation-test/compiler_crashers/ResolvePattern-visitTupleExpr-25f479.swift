// {"kind":"typecheck","original":"dab09662","signature":"(anonymous namespace)::ResolvePattern::visitTupleExpr(swift::TupleExpr*)","signatureAssert":"Assertion failed: (!elts[0].getLabel().empty()), function create","signatureNext":"ResolvePattern::composeTupleOrParenPattern"}
// RUN: not --crash %target-swift-frontend -typecheck %s
if case .a((repeat b)) = <#expression#> {
}
