// {"kind":"typecheck","original":"d41118a5","signature":"swift::ArgumentList::forImplicitCallTo(swift::ParameterList*, llvm::ArrayRef<swift::Expr*>, swift::ASTContext&)","signatureAssert":"Assertion failed: (param->isInOut() == argExprs[idx]->isSemanticallyInOutExpr()), function forImplicitCallTo","signatureNext":"buildStorageReference"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a < b {wrappedValue : b static subscript(_enclosingInstance c wrapped d , inout storage e : f
}
class g {
  @a bandwidth : Int
