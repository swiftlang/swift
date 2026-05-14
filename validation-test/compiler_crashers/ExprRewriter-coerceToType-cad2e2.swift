// {"kind":"typecheck","original":"0b337f68","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)::$_3::operator()(swift::Type, swift::Type) const","signatureAssert":"Assertion failed: (restriction == ConversionRestrictionKind::DeepEquality), function operator()","signatureNext":"ExprRewriter::buildSingleCurryThunk"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  func c(
    d: inout b)
  struct e<f: a> {
    var i: f
    func g() where h == f.b {
      i.c
    }
  }
}
