// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::setTargetFor(swift::constraints::SyntacticElementTargetKey, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (inserted), function setTargetFor"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{$0=[{} }(
[a:
