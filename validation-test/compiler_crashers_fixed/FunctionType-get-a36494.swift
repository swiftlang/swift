// {"kind":"typecheck","original":"6af182ec","signature":"swift::FunctionType::get(llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, std::__1::optional<swift::ASTExtInfo>)","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function FunctionType","signatureNext":"ConstraintSystem::getOpenedStorageType"}
// RUN: not %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b> {
  wrappedValue : b  static subscript<
    c: Actor
  >(_enclosingInstance d: isolated c, wrapped e: ReferenceWritableKeyPath<c, b>,
    storage f: ReferenceWritableKeyPath<c, a>
  ) -> b
}
actor g {
  @a var h: String
}
