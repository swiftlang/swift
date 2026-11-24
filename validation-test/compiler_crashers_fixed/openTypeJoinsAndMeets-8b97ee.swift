// {"kind":"typecheck","original":"e0ea2325","signature":"swift::constraints::openTypeJoinsAndMeets(swift::constraints::ConstraintSystem&, swift::Type, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (!type->is<JoinType>() && !type->is<MeetType>()), function openTypeJoinsAndMeets","signatureNext":"TypeVariableBinding::attempt"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a: ExpressibleByIntegerLiteral {
  struct b<c: a> {
    d{ c() ?? c ?? c(e: 0.0) ?? f
      }
  }
