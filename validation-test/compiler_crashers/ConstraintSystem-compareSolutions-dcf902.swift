// {"kind":"typecheck","original":"2e9096bf","signature":"swift::constraints::ConstraintSystem::compareSolutions(swift::constraints::ConstraintSystem&, llvm::ArrayRef<swift::constraints::Solution>, swift::constraints::SolutionDiff const&, unsigned int, unsigned int)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension Sequence {
    b<c, d: KeyPath<Element, c>>(d )
  {
    [(a:2), (0.0      a : 1)].b(\.a
