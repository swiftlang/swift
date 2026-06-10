// {"kind":"typecheck","original":"d604df44","signature":"swift::LValueType::get(swift::Type)","signatureAssert":"Assertion failed: (!objectTy->is<LValueType>() && !objectTy->is<InOutType>() && \"cannot have 'inout' or @lvalue wrapped inside an @lvalue\"), function get","signatureNext":"ConstraintSystem::simplifyLValueObjectConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  var b {
    mutate {
      &c = d
    }
  }
}
