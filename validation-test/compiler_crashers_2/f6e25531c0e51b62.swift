// {"signature":"swift::constraints::RequirementFailure::RequirementFailure(swift::constraints::Solution const&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{associatedtype b} extension a where b == {
  protocol c var d : c {
    d
