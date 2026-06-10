// {"kind":"typecheck","original":"18f513af","signature":"swift::TypeChecker::typeCheckBinding(swift::Pattern*&, swift::Expr*&, swift::DeclContext*, swift::Type, swift::PatternBindingDecl*, unsigned int, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>)"}
// RUN: not %target-swift-frontend -typecheck %s
{
  guard let a = a   "
