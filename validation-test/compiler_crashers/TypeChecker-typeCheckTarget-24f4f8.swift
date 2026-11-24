// {"kind":"typecheck","original":"dc8eb996","signature":"swift::TypeChecker::typeCheckTarget(swift::constraints::SyntacticElementTarget&, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>, swift::DiagnosticTransaction*)","signatureNext":"TypeChecker::typeCheckExpression"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b, each c>(d: repeat each b
repeat each c
)
-> (repeat(each b(repeat each c)))
func e<each g>(f: repeat each g) {
  a(d: repeat each f
