// {"signature":"(anonymous namespace)::Verifier::verifyParsed(swift::AbstractClosureExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func == {
  switch {
  case.a {
