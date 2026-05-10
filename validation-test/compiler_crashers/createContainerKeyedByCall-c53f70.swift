// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"a349cfb8","signature":"createContainerKeyedByCall(swift::ASTContext&, swift::DeclContext*, swift::Expr*, swift::Type, swift::NominalTypeDecl*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"deriveBodyEncodable_encode"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
struct a: Codable {
  enum CodingKeys<b>: CodingKey {
  }
}
