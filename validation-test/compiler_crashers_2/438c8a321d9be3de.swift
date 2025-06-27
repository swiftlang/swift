// {"signature":"swift::constraints::SameTypeRequirementFailure::SameTypeRequirementFailure(swift::constraints::Solution const&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a struct b extension Dictionary : a where Value == let func !c {
  let d : [Int:b] let : a = d
