// {"kind":"typecheck","original":"dc8eb996","signature":"swift::TypeChecker::typeCheckTarget(swift::constraints::SyntacticElementTarget&, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>, swift::DiagnosticTransaction*)","signatureNext":"TypeChecker::typeCheckExpression"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b, each c>(d: repeat each b
repeat each c
)
-> (repeat(each b(repeat each c)))
func e<each g>(f: repeat each g) {
  a(d: repeat each f
