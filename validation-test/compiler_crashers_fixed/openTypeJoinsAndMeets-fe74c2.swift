// {"kind":"typecheck","original":"fb808a39","signature":"swift::constraints::openTypeJoinsAndMeets(swift::constraints::ConstraintSystem&, swift::Type, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (!type->is<JoinType>() && !type->is<MeetType>()), function openTypeJoinsAndMeets","signatureNext":"TypeVariableBinding::attempt"}
// RUN: not %target-swift-frontend -typecheck %s
struct a {
  b { a ?? a ?? 0.a ?? 0?
  }
