// {"kind":"typecheck","original":"d5861276","signature":"swift::constraints::ConstraintSystem::repairFailures(swift::Type, swift::Type, swift::constraints::ConstraintKind, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, llvm::SmallVectorImpl<swift::constraints::RestrictionOrFix>&, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"ConstraintSystem::matchTypes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
actor a {
    f<each b, c>(
      repeat each b,  () -> c,
      g: c -> Int = f {
      } g: {
        d, e -> Int
