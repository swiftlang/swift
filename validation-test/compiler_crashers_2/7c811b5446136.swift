// {"signature":"swift::constraints::ConstraintSystem::matchTypes(swift::Type, swift::Type, swift::constraints::ConstraintKind, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
weak var a : b? a
class c
  class b
    weak var a: c?
