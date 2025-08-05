// {"kind":"typecheck","signature":"swift::NominalType::get(swift::NominalTypeDecl*, swift::Type, swift::ASTContext const&)","signatureAssert":"Assertion failed: ((!Parent || Parent->is<NominalType>() || Parent->is<BoundGenericType>() || Parent->is<UnboundGenericType>()) && \"parent must be a nominal type\"), function get"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension() {
  struct a extension a
