// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::applySolutionFixes(swift::constraints::Solution const&)","signatureNext":"ConstraintSystem::applySolution"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b {
  c<each d >(repeat each d, e: (
}
func bar(a: b) {
  a.c(e : ""
