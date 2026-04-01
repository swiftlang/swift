// {"kind":"typecheck","original":"1bb8c42a","signature":"swift::DeclContext::getASTContext() const","signatureNext":"AbstractFunctionDecl::getOriginalBodySourceRange"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@a({
  func b()
})
let c
