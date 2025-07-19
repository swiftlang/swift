// {"signature":"swift::constraints::ArgumentMismatchFailure::diagnoseAttemptedRegexBuilder() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b {
  c<each d >(repeat each d, e: (
}
func bar(a: b) {
  a.c(e : ""
