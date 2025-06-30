// {"signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)::$_3::operator()(swift::Type, swift::Type) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(inout _ )
var b = String
a(b
