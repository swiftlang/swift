// {"kind":"typecheck","original":"1309b52b","signature":"swift::TypeChecker::typeCheckBinding(swift::Pattern*&, swift::Expr*&, swift::DeclContext*, swift::Type, swift::PatternBindingDecl*, unsigned int, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>)"}
// RUN: not %target-swift-frontend -typecheck %s
{
  let a = 0.0
  switch a {
  case let !b where b:
  }
}
