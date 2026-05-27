// {"kind":"typecheck","original":"f1fa1190","signature":"(anonymous namespace)::DeclChecker::visit(swift::Decl*)","signatureNext":"TypeChecker::typeCheckDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  @a var c = b {
    @storageRestrictions(initializes: _c) init
  }
}
