// {"kind":"typecheck","signature":"swift::constraints::ArgumentMismatchFailure::buildInfo(swift::constraints::Solution const&, swift::constraints::ConstraintLocator*)","signatureNext":"ArgumentMismatchFailure::create"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b {
  c<each d >(repeat each d, e: (
}
func bar(a: b) {
  a.c(e : ""
