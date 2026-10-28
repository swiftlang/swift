// {"kind":"typecheck","original":"e8c6fd15","signature":"(anonymous namespace)::DeclChecker::visit(swift::Decl*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"DeclChecker::visitExtensionDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  struct b<c> {
    extension <#type#> {
      struct d {
        subscript() -> c
      }
    }
  }
}
