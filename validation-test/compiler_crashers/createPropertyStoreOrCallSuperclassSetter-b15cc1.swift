// {"kind":"typecheck","original":"1098007b","signature":"createPropertyStoreOrCallSuperclassSetter(swift::AccessorDecl*, swift::Expr*, swift::AbstractStorageDecl*, (anonymous namespace)::TargetImpl, llvm::SmallVectorImpl<swift::ASTNode>&, swift::ASTContext&)","signatureAssert":"Assertion failed: (destType->getOptionalObjectType()->isEqual(value->getType())), function createPropertyStoreOrCallSuperclassSetter","signatureNext":"synthesizeTrivialSetterBodyWithStorage"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b, c> {
  wrappedValue: [c: b]  static subscript<d>(_enclosingInstance e: d, wrapped f: ReferenceWritableKeyPath<d, [c: b]>,
    storage g: ReferenceWritableKeyPath<d, Self>
  ) -> b?
}
class h {
  @a var i: [String: Int]
}
