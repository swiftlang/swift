// {"kind":"typecheck","original":"2085cf11","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"Solution::coerceToType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  var b: (borrowing Int) -> Int
  let <#pattern#>: WritableKeyPath<a, (Int) -> Int> = \b
}
