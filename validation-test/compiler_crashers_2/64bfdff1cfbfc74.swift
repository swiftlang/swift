// {"signature":"swift::constraints::ConstraintSystem::simplifySameShapeConstraint(swift::Type, swift::Type, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
struct a < each b {
  c(d : repeat each b) {
    c(preacidte : b) {
