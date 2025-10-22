// {"kind":"typecheck","original":"06d6e891","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (toType && !toType->hasError() && !toType->hasTypeVariableOrPlaceholder()), function coerceToType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b {
  @propertyWrapper struct f<b {
    wrappedValue : b    var projectedValue: a<b>  init(projectedValue:
  }
    c(@f arg: Int)
  func d(e: a<Int>) {
    c($arg: e
