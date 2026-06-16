// {"kind":"typecheck","original":"f6c0ad1d","signature":"swift::constraints::ConstraintSystem::lookupDependentMember(swift::Type, swift::AssociatedTypeDecl*, bool, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (!base->isTypeVariableOrMember() && \"Must simplify first\"), function lookupDependentMember","signatureNext":"ConstraintSystem::simplifyForEachElementConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
for await in a(
  struct a<b: AsyncSequence>: AsyncSequence {
makeAsyncIterator->b.AsyncIterator
