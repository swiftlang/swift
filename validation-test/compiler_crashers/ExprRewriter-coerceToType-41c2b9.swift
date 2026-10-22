// {"frontendArgs":["-typecheck","-enable-experimental-feature","KeyPathWithMethodMembers"],"kind":"custom","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (toType && !toType->hasError() && !toType->hasTypeVariableOrHole() && !toType->hasPlaceholder()), function coerceToType","signatureNext":"ExprRewriter::coerceCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck -enable-experimental-feature KeyPathWithMethodMembers %s
// REQUIRES: swift_feature_KeyPathWithMethodMembers
struct a {
b<c>(c) {
\a.b(1)
