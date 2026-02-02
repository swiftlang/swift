// {"kind":"typecheck","original":"22c204bc","signature":"swift::TypeChecker::typeCheckBinding(swift::Pattern*&, swift::Expr*&, swift::DeclContext*, swift::Type, swift::PatternBindingDecl*, unsigned int, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>)"}
// RUN: not %target-swift-frontend -typecheck %s
struct a: ExpressibleByIntegerLiteral
  func *** (a , a )
}
{
  let b = 3 *** 4
