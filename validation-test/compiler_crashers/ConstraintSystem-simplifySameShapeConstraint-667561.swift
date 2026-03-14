// {"kind":"typecheck","original":"2da2887e","signature":"swift::constraints::ConstraintSystem::simplifySameShapeConstraint(swift::Type, swift::Type, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b {
  init(repeat each b)
  func c -> a {
    a(
