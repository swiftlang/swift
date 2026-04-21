// {"kind":"typecheck","original":"a3a712b1","signature":"swift::constraints::MissingArgumentsFailure::diagnoseClosure(swift::ClosureExpr const*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"MissingArgumentsFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a() -> (((), Void) -> Void)? {
  {
    {
    }
  }(<#expression#>)
}
