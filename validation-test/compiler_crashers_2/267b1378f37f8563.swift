// {"kind":"typecheck","signature":"swift::constraints::AllowArgumentMismatch::create(swift::constraints::ConstraintSystem&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b {
  c<each d >(repeat each d, e: (
}
func bar(a: b) {
  a.c(e : ""
