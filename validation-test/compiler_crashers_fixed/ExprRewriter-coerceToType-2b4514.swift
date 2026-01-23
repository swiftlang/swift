// {"kind":"typecheck","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not %target-swift-frontend -typecheck %s
func a<each b>(repeat each b) -> some Collection<(repeat each b)>
_ = a().first
