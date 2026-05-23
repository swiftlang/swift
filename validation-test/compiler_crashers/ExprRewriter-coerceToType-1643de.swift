// {"kind":"typecheck","original":"be86e682","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)::$_3::operator()(swift::Type, swift::Type) const","signatureAssert":"Assertion failed: (restriction == ConversionRestrictionKind::DeepEquality), function operator()","signatureNext":"ExprRewriter::coerceCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  var c: b {
    set
  }
}
func d<e: a>(f: inout e, delta: Int) where e.b == <#type#> {
  f.c += delta
}
