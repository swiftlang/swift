// {"kind":"typecheck","original":"aae5b261","signature":"createContainerKeyedByCall(swift::ASTContext&, swift::DeclContext*, swift::Expr*, swift::Type, swift::NominalTypeDecl*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"deriveBodyDecodable_init"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a: Codable {
  var b: Int
  var c: String
  enum CodingKeys<d>: CodingKey {
    case b
    case c
  }
}
