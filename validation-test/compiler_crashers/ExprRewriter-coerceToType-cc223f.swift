// {"kind":"typecheck","extraArgs":["-solver-disable-diagnose-valid-salvage"],"original":"04d0d223","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)::$_3::operator()(swift::Type, swift::Type) const","signatureAssert":"Assertion failed: (restriction == ConversionRestrictionKind::DeepEquality), function operator()","signatureNext":"ExprRewriter::coerceCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck -solver-disable-diagnose-valid-salvage %s
protocol a {
  associatedtype b
}
extension Int: a {
  struct c<d: a> {
    var e: d.b?
    f {
    var g = c<Int>() g.e? += 0
      }
  }
