// {"kind":"typecheck","original":"43bd6d32","signature":"swift::TypeChecker::typeCheckTarget(swift::constraints::SyntacticElementTarget&, swift::optionset::OptionSet<swift::TypeCheckExprFlags, unsigned int>, swift::DiagnosticTransaction*)","signatureNext":"StmtChecker::typeCheckStmt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<each b, each c> = (repeat (each b each c)
0
func d<each b, each c>(repeat each b, repeat each c) -> a<repeat each b, repeat each c>
func e<each g>(f: repeat each g) -> (repeat () -> each g) {
  (repeat d {
    each f
