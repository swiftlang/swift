// {"kind":"typecheck","original":"6cb6108c","signature":"(anonymous namespace)::ExprRewriter::coerceCallArguments(swift::ArgumentList*, swift::AnyFunctionType*, swift::ConcreteDeclRef, swift::ApplyExpr*, swift::constraints::ConstraintLocatorBuilder, llvm::ArrayRef<swift::AppliedPropertyWrapper>)","signatureAssert":"Assertion failed: (appliedWrapperIndex == appliedPropertyWrappers.size()), function coerceCallArguments","signatureNext":"ExprRewriter::finishApply"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b> {
  var wrappedValue: b
}
func c<b>(d: inout b) {
  { (@a ) in
  }(d)
}
