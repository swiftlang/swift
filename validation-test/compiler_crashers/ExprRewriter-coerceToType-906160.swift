// {"kind":"typecheck","original":"2085cf11","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"Solution::coerceToType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  var b: (borrowing Int) -> Int
  let <#pattern#>: WritableKeyPath<a, (Int) -> Int> = \b
}
