// {"kind":"typecheck","signature":"swift::TypeChecker::typeCheckTarget(swift::constraints::SyntacticElementTarget&, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>, swift::DiagnosticTransaction*)","signatureNext":"TypeChecker::typeCheckExpression"}
// RUN: not %target-swift-frontend -typecheck %s
{
  (a: _) in
  struct b {
    var c = a
  }
  b.c
}
