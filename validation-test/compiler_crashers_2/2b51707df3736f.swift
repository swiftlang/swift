// {"signature":"swift::constraints::ConstraintSystem::recordMatchCallArgumentResult(swift::constraints::ConstraintLocator*, swift::constraints::MatchCallArgumentResult)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
if
case.a(a.b) = {
