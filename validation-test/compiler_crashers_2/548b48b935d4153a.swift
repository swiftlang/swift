// {"signature":"swift::NominalType::get(swift::NominalTypeDecl*, swift::Type, swift::ASTContext const&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension() {
  struct a extension a
