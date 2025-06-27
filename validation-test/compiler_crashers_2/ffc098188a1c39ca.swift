// {"signature":"createPropertyStoreOrCallSuperclassSetter(swift::AccessorDecl*, swift::Expr*, swift::AbstractStorageDecl*, (anonymous namespace)::TargetImpl, llvm::SmallVectorImpl<swift::ASTNode>&, swift::ASTContext&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a < b {
  wrappedValue : b static subscript<c>(_enclosingInstance d
                                       : c, wrapped e
                                       : ReferenceWritableKeyPath<c, b>
                                           storage f
                                       : ReferenceWritableKeyPath<c, Self>) Self
} class g {
  @a h = 7
