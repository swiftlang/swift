// {"kind":"typecheck","original":"16a71758","signature":"swift::TypeChecker::typeCheckBinding(swift::Pattern*&, swift::Expr*&, swift::DeclContext*, swift::Type, swift::PatternBindingDecl*, unsigned int, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>)","signatureNext":"TypeChecker::typeCheckPatternBinding"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(
    = {
      let b = c {
      } {
        func d -> some Any
