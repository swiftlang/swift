// {"kind":"typecheck","original":"25782df5","signature":"(anonymous namespace)::SyntacticElementConstraintGenerator::visitDecl(swift::Decl*)","signatureAssert":"Assertion failed: (!projectedTy->hasUnboundGenericType() && !projectedTy->hasPlaceholder()), function setPropertyWrapperAuxiliaryTypes"}
// RUN: not %target-swift-frontend -typecheck %s
@propertyWrapper
struct a<
  b
> {
  projectedValue : Self  var wrappedValue: b
}
{
  @a var c: <#type#>
}
