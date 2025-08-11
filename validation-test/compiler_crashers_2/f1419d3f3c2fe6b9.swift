// {"kind":"typecheck","signature":"swift::Mangle::ASTMangler::appendExtension(swift::ExtensionDecl const*, swift::Mangle::ASTMangler::BaseEntitySignature&, llvm::StringRef)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension Collection where Self : a {
  struct Index protocol a
