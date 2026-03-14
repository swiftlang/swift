// {"kind":"typecheck","original":"deecafdd","signature":"swift::TypeChecker::typeCheckBinding(swift::Pattern*&, swift::Expr*&, swift::DeclContext*, swift::Type, swift::PatternBindingDecl*, unsigned int, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>)"}
// RUN: not %target-swift-frontend -typecheck %s
return {
  lazy var a = <#expression#>
}
