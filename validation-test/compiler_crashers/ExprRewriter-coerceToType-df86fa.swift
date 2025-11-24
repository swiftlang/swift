// {"kind":"typecheck","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
    callAsFunction( () -> Void)
  _ = a.  init {
