// {"kind":"typecheck","signature":"swift::CallerSideDefaultArgExprRequest::evaluate(swift::Evaluator&, swift::DefaultArgumentExpr*) const","signatureAssert":"Assertion failed: (ctx.Diags.hadAnyError()), function evaluate","signatureNext":"CallerSideDefaultArgExprRequest::OutputType"}
// RUN: not %target-swift-frontend -typecheck %s
@propertyWrapper
struct a {
  var wrappedValue: Int?
  var projectedValue: a {
  }
  init(projectedValue: a)
}
func b(@a c: Int? = nil)
b()
