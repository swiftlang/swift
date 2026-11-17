// {"kind":"typecheck","original":"d782036f","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)::$_3::operator()(swift::Type, swift::Type) const","signatureAssert":"Assertion failed: (restriction == ConversionRestrictionKind::DeepEquality), function operator()","signatureNext":"ExprWalker::rewriteTarget"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b> {
  subscript(e: repeat each b) -> (repeat each b) {
    _modify {
      var c = (d:repeat each e)
      yield c
    }
  }
}
