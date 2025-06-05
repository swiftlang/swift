// {"signature":"(anonymous namespace)::Verifier::verifyParsed(swift::AbstractClosureExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
func == {
  switch {
  case.a {
