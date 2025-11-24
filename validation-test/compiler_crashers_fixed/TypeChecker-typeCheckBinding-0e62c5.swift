// {"kind":"typecheck","original":"e679747c","signature":"swift::TypeChecker::typeCheckBinding(swift::Pattern*&, swift::Expr*&, swift::DeclContext*, swift::Type, swift::PatternBindingDecl*, unsigned int, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>)"}
// RUN: not %target-swift-frontend -typecheck %s
enum a<b>: Error {
  case c(
}
struct d {
    bar( Error? -> Void)
  {
    bar {
      e in
      return if let a<Int>.c(f) = e {
        f
