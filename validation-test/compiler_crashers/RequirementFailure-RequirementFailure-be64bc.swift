// {"kind":"typecheck","signature":"swift::constraints::RequirementFailure::RequirementFailure(swift::constraints::Solution const&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (AffectedDecl), function RequirementFailure"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  struct b<c
    extension b where c: a {
      typealias d<c> =
    }
      b<Int>
.d
