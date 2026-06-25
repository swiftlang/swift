// {"kind":"typecheck","original":"2d952233","signature":"swift::TypeChecker::typeCheckTarget(swift::constraints::SyntacticElementTarget&, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>, swift::DiagnosticTransaction*)","signatureNext":"TypeChecker::typeCheckExpression"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each c, each d {
    e(repeat (each c, each d))
    f<each c, each d>(repeat each c, b: repeat each d)
    -> a<repeat each c, repeat each d>
  {
    h
    f(repeat each b).e
