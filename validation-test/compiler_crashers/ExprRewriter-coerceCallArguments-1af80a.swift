// {"kind":"typecheck","signature":"(anonymous namespace)::ExprRewriter::coerceCallArguments(swift::ArgumentList*, swift::AnyFunctionType*, swift::ConcreteDeclRef, swift::ApplyExpr*, swift::constraints::ConstraintLocatorBuilder, llvm::ArrayRef<swift::AppliedPropertyWrapper>)","signatureAssert":"Assertion failed: (solution.argumentMatchingChoices.count(locatorPtr) == 1), function coerceCallArguments","signatureNext":"ExprRewriter::buildKeyPathSubscriptComponent"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup
struct S {
  subscript(dynamicMember member: String, x: Int = 0) -> Int {
    0
  }
}
_ = \S.foo
